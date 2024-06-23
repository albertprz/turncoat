module Utils.TranspositionTable where

import           AppPrelude

import           Models.Command
import           Models.Move
import           Models.Position              (ZKey (..))
import           Models.Score
import           Utils.Board

import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Word
import           Foreign.Storable.Generic
import           Test.QuickCheck


type TTable = IOVector StorableTEntry


data TEntry = TEntry {
  zobristKey :: ZKey,
  bestMove   :: Maybe Move,
  score      :: Score,
  depth      :: Depth,
  nodeType   :: NodeType,
  age        :: Age
} deriving (Eq, Show, Generic)

instance Arbitrary TEntry where
  arbitrary = TEntry <$> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary
                     <*> arbitrary <*> arbitrary

data StorableTEntry = StorableTEntry {
  zobristKey :: ZKey,
  info       :: Word64
} deriving Generic

instance GStorable StorableTEntry


tTableSize :: (?opts :: EngineOptions) => Word64
tTableSize | ?opts.hashSize == 0 = 0
           | otherwise          = toBoard bits
  where
    bits = msb (fromIntegral ?opts.hashSize) + 16


hashZKey :: (?opts :: EngineOptions) => ZKey -> Int
hashZKey (ZKey zKey) =
  fromIntegral (zKey % tTableSize)


encodeTEntry :: TEntry -> StorableTEntry
encodeTEntry TEntry {..} = StorableTEntry {
  zobristKey = zobristKey,
  info = fromIntegral bestMoveN
    .| fromIntegral (fromIntegral score :: Word16) << 32
    .| fromIntegral depth << 48
    .| fromIntegral nodeTypeN << 56
    .| fromIntegral age << 58
  }
  where
    StorableMove bestMoveN = encodeMove bestMove
    NodeType nodeTypeN     = nodeType


decodeTEntry :: StorableTEntry -> Maybe TEntry
decodeTEntry StorableTEntry {..}
  | testSquare info 63 = Nothing
  | otherwise          = Just TEntry {
      zobristKey = zobristKey
      , bestMove   = decodeMove $ fromIntegral info
      , score      = fromIntegral (info >> 32)
      , depth      = fromIntegral (info >> 48)
      , nodeType   = fromIntegral ((info >> 56) & 3)
      , age        = fromIntegral ((info >> 58) & 31)
  }


lookupEntry
  :: (?tTable :: TTable, ?opts :: EngineOptions) => ZKey -> IO (Maybe TEntry)
lookupEntry !zKey
  | tTableSize == 0 = pure Nothing
  | otherwise      = do
  entry <- decodeTEntry <$> Vector.unsafeRead ?tTable (hashZKey zKey)
  pure (maybeFilter ((== zKey) . (.zobristKey)) entry)


lookupScore
  :: (?tTable :: TTable, ?opts :: EngineOptions)
  => Score -> Score -> Depth -> ZKey -> IO (Maybe (Score, Maybe Move))
lookupScore !alpha !beta !depth !zKey = do
  entry <- maybeFilter ((>= depth) . (.depth)) <$> lookupEntry zKey
  let !score = getScore =<< entry
      !bestMove = (.bestMove) =<< entry
  pure ((, bestMove) <$> score)
  where
    getScore TEntry {score, nodeType} =
      case nodeType of
        PV                   -> Just score
        Cut | score >= beta  -> Just beta
        All | score <= alpha -> Just alpha
        _                    -> Nothing


lookupBestMove
  :: (?tTable :: TTable, ?opts :: EngineOptions) => ZKey -> IO (Maybe Move)
lookupBestMove !zKey = do
  entry <- lookupEntry zKey
  pure ((.bestMove) =<< entry)


insert
  :: (?tTable :: TTable, ?opts :: EngineOptions) => ZKey -> TEntry -> IO ()
insert !zKey !newEntry
 | tTableSize == 0 = pure ()
 | otherwise       = do
  entry <- decodeTEntry <$> Vector.unsafeRead ?tTable hashKey
  when (isStaleEntry entry newEntry)
    $ Vector.unsafeWrite ?tTable hashKey (encodeTEntry newEntry)
  where
    hashKey = hashZKey zKey


isStaleEntry :: Maybe TEntry -> TEntry -> Bool
isStaleEntry (Just entry) newEntry =
  newEntry.age - entry.age > 2
  || newEntry.depth >= entry.depth
  && (newEntry.nodeType == PV || entry.nodeType /= PV)
isStaleEntry Nothing _ =
  True


emptyTEntry :: StorableTEntry
emptyTEntry = StorableTEntry {
  zobristKey = 0,
  info       = toBoard 63
}


create ::  EngineOptions -> IO TTable
create opts = Vector.replicate (fromIntegral tTableSize) emptyTEntry
  where
    ?opts = opts


reset :: TTable -> IO ()
reset = flip Vector.set emptyTEntry


clear :: TTable -> IO ()
clear = Vector.clear
