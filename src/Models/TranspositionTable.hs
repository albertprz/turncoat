module Models.TranspositionTable where

import           AppPrelude

import           Data.HashTable.IO (LinearHashTable)
import qualified Data.HashTable.IO as HashTable
import           Models.Move
import           Models.Score


type TTable = LinearHashTable ZKey TEntry

data TEntry = TEntry {
  depth      :: Int,
  bestMove   :: Maybe Move,
  score      :: Score,
  nodeType   :: NodeType,
  zobristKey :: ZKey
}

newtype ZKey = ZKey Word64
  deriving (Eq, Ord, Num, Hashable)

{-# INLINE  lookupEntry #-}
lookupEntry :: (?tTable :: TTable) => ZKey -> IO (Maybe TEntry)
lookupEntry zKey = do
  entry <- HashTable.lookup ?tTable zKey
  pure (maybeFilter ((== zKey) . (.zobristKey)) entry)

{-# INLINE  lookupScore #-}
lookupScore :: (?tTable :: TTable) => Score -> Score -> Int -> ZKey -> IO (Maybe Score)
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
insert = HashTable.insert ?tTable


create :: Int -> IO TTable
create = HashTable.newSized
