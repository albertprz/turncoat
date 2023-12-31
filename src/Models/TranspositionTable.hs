module Models.TranspositionTable where

import           AppPrelude

import           Data.HashTable.IO (LinearHashTable)
import qualified Data.HashTable.IO as HashTable
import           Models.Move
import           Models.Score


type TTable = LinearHashTable ZKey TEntry

data TEntry = TEntry {
  depth    :: Int,
  bestMove :: Move,
  score    :: Score,
  nodeType :: NodeType
}

newtype ZKey = ZKey Word64
  deriving (Eq, Ord, Num, Hashable)


{-# INLINE  lookup #-}
lookup :: MonadIO m => (?tTable :: TTable) => Int -> ZKey -> m (Maybe TEntry)
lookup depth zKey = liftIO do
  entry <- HashTable.lookup ?tTable zKey
  pure $ maybeFilter ((>= depth) . (.depth)) entry


{-# INLINE  insert #-}
insert :: MonadIO m => (?tTable :: TTable) => ZKey -> TEntry -> m ()
insert zKey tEntry = liftIO
  $ HashTable.insert ?tTable zKey tEntry
