module Models.TranspositionTable where

import           AppPrelude

import           Data.HashTable.IO (LinearHashTable)
import qualified Data.HashTable.IO as HashTable
import           Models.Move
import           Models.Score


type TTable = LinearHashTable ZKey TEntry

data TEntry = TEntry {
  depth    :: Int,
  bestMove :: Maybe Move,
  score    :: Score,
  nodeType :: NodeType
}

newtype ZKey = ZKey Word64
  deriving (Eq, Ord, Num, Hashable)


{-# INLINE  lookup #-}
lookup :: (?tTable :: TTable) => Int -> ZKey -> IO (Maybe TEntry)
lookup depth zKey = do
    entry <- HashTable.lookup ?tTable zKey
    pure $! maybeFilter ((>= depth) . (.depth)) entry


{-# INLINE  insert #-}
insert :: (?tTable :: TTable) => ZKey -> TEntry -> IO ()
insert = HashTable.insert ?tTable

create :: Int -> IO TTable
create = HashTable.newSized
