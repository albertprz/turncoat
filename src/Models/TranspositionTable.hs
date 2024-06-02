module Models.TranspositionTable where

import           AppPrelude

import           Foreign.Storable.Generic

import           Constants.Boards
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import           GHC.Bits
import           GHC.Word                     (Word16)
import           Models.Move
import           Models.Score
import           Test.QuickCheck              (Arbitrary (..))

type TTable = IOVector StorableTEntry


data TEntry = TEntry {
  zobristKey :: ZKey,
  bestMove   :: Maybe Move,
  score      :: Score,
  depth      :: Depth,
  nodeType   :: NodeType
} deriving (Eq, Show, Generic)

instance Arbitrary TEntry where
  arbitrary = TEntry <$> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary <*> arbitrary

data StorableTEntry = StorableTEntry {
  zobristKey :: ZKey,
  info       :: Word64
} deriving Generic

instance GStorable StorableTEntry

newtype ZKey = ZKey Word64
  deriving (Eq, Show, Generic, Ord, Num, Hashable, Storable, Arbitrary)


tTableSize :: Word64
tTableSize = bit 26

hashZKey :: ZKey -> Int
hashZKey (ZKey zKey) =
  fromIntegral $ mod zKey tTableSize

encodeTEntry :: TEntry -> StorableTEntry
encodeTEntry TEntry {..} = StorableTEntry {
  zobristKey = zobristKey,
  info = fromIntegral bestMoveN
    .|. fromIntegral (fromIntegral score :: Word16) << 32
    .|. fromIntegral depth << 48
    .|. fromIntegral nodeTypeN << 56
}
  where
    StorableMove bestMoveN = encodeMove bestMove
    NodeType nodeTypeN = nodeType

decodeTEntry :: StorableTEntry -> Maybe TEntry
decodeTEntry StorableTEntry {..}
  | testSquare info 63 = Nothing
  | otherwise = Just TEntry {
      zobristKey = zobristKey,
      bestMove = decodeMove $ fromIntegral info,
      score = fromIntegral (info >> 32),
      depth = fromIntegral (info >> 48),
      nodeType = fromIntegral ((info >> 56) .&. 3)
    }


lookupEntry :: (?tTable :: TTable) => ZKey -> IO (Maybe TEntry)
lookupEntry !zKey = do
  entry <- decodeTEntry <$> Vector.unsafeRead ?tTable (hashZKey zKey)
  pure (maybeFilter ((== zKey) . (.zobristKey)) entry)

lookupScore :: (?tTable :: TTable) => Score -> Score -> Depth -> ZKey -> IO (Maybe Score)
lookupScore !alpha !beta !depth !zKey = do
  entry <- lookupEntry zKey
  pure (getScore =<< maybeFilter ((>= depth) . (.depth)) entry)
  where
    getScore TEntry {score, nodeType} =
      case nodeType of
        PV                   -> Just score
        Cut | score >= beta  -> Just beta
        All | score <= alpha -> Just alpha
        _                    -> Nothing


lookupBestMove :: (?tTable :: TTable) => ZKey -> IO (Maybe Move)
lookupBestMove !zKey = do
  entry <- lookupEntry zKey
  pure ((.bestMove) =<< entry)


insert :: (?tTable :: TTable) => ZKey -> TEntry -> IO ()
insert !zKey !entry =
  Vector.unsafeWrite ?tTable (hashZKey zKey) (encodeTEntry entry)

emptyTEntry :: StorableTEntry
emptyTEntry = StorableTEntry {
  zobristKey = 0,
  info = bit 63
}

create :: IO TTable
create = Vector.replicate (fromIntegral tTableSize) emptyTEntry

clear ::  TTable -> IO ()
clear = Vector.clear
