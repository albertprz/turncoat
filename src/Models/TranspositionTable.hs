module Models.TranspositionTable where

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


tTableSize :: (?opts :: EngineOptions) => Word64
tTableSize = toBoard bits
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
      nodeType = fromIntegral ((info >> 56) & 3)
    }


lookupEntry
  :: (?tTable :: TTable, ?opts :: EngineOptions) => ZKey -> IO (Maybe TEntry)
lookupEntry !zKey = do
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
insert !zKey !entry =
  Vector.unsafeWrite ?tTable (hashZKey zKey) (encodeTEntry entry)


emptyTEntry :: StorableTEntry
emptyTEntry = StorableTEntry {
  zobristKey = 0,
  info = toBoard 63
}


create :: (?opts :: EngineOptions) => IO TTable
create = Vector.replicate (fromIntegral tTableSize) emptyTEntry


clear ::  TTable -> IO ()
clear = Vector.clear
