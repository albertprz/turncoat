module Models.Score where

import           AppPrelude

newtype Score = Score Int
  deriving (Eq, Ord, Num, Bounded, Storable)

newtype NodeType = NodeType Int
  deriving (Eq, Ord, Num)

{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2
