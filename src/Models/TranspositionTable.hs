module Models.TranspositionTable where

import           AppPrelude

import           Foreign.Storable.Generic

import           Constants.Boards
import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import           GHC.Bits
import           Models.Move
import           Models.Score

type TTable = IOVector StorableTEntry


data TEntry = TEntry {
  zobristKey :: ZKey,
  bestMove   :: Maybe Move,
  score      :: Score,
  depth      :: Depth,
  nodeType   :: NodeType
}

data StorableTEntry = StorableTEntry {
  zobristKey :: ZKey,
  info       :: Word64
} deriving Generic

instance GStorable StorableTEntry

newtype ZKey = ZKey Word64
  deriving (Eq, Ord, Num, Hashable, Storable)


emptyStorableTEntry :: StorableTEntry
emptyStorableTEntry = StorableTEntry {
  zobristKey = 0,
  info = 1 << 63
}

tTableSize :: Word64
tTableSize = 1 << 26

{-# INLINE  hashZKey #-}
hashZKey :: ZKey -> Int
hashZKey (ZKey zKey) =
  fromIntegral $ mod zKey tTableSize

{-# INLINE  encodeTEntry #-}
encodeTEntry :: TEntry -> StorableTEntry
encodeTEntry TEntry {..} = StorableTEntry {
  zobristKey = zobristKey,
  info = fromIntegral bestMove'
    .|. fromIntegral score' << 32
    .|. fromIntegral depth' << 48
    .|. fromIntegral nodeType' << 54
}
  where
    StorableMove bestMove' = encodeMove bestMove
    Score score' = score
    Depth depth' = depth
    NodeType nodeType' = nodeType

{-# INLINE  decodeTEntry #-}
decodeTEntry :: StorableTEntry -> Maybe TEntry
decodeTEntry StorableTEntry {..}
  | testBit info 63 = Nothing
  | otherwise = Just TEntry {
      zobristKey = zobristKey,
      bestMove = decodeMove $ fromIntegral info,
      score = fromIntegral (info >> 32),
      depth = fromIntegral (info >> 48),
      nodeType = fromIntegral (info >> 54)
    }


{-# INLINE  lookupEntry #-}
lookupEntry :: (?tTable :: TTable) => ZKey -> IO (Maybe TEntry)
lookupEntry zKey = do
  entry <- decodeTEntry <$> Vector.unsafeRead ?tTable (hashZKey zKey)
  pure (maybeFilter ((== zKey) . (.zobristKey)) entry)

{-# INLINE  lookupScore #-}
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


{-# INLINE  lookupBestMove #-}
lookupBestMove :: (?tTable :: TTable) => ZKey -> IO (Maybe Move)
lookupBestMove !zKey = do
  entry <- lookupEntry zKey
  pure $! ((.bestMove) =<< entry)


{-# INLINE  insert #-}
insert :: (?tTable :: TTable) => ZKey -> TEntry -> IO ()
insert zKey entry =
  Vector.unsafeWrite ?tTable (hashZKey zKey) (encodeTEntry entry)


create :: IO TTable
create = Vector.new (fromIntegral tTableSize)
