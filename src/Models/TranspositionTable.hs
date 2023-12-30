module Models.TranspositionTable where

import           AppPrelude

import           Data.HashTable.ST.Linear
import           Models.Move
import           Models.Score


type TTable s = HashTable s ZKey TEntry

data TEntry = TEntry {
  zobristKey :: ZKey,
  depth      :: Int,
  move       :: Move,
  score      :: Score,
  nodeType   :: NodeType
}


newtype ZKey = ZKey Word64
  deriving (Eq, Ord, Num, Hashable)
