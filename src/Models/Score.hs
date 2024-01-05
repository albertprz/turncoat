module Models.Score where

import           AppPrelude
import           GHC.Word   (Word16)

newtype Score = Score Word16
  deriving (Eq, Ord, Num, Enum, Bounded, Storable)

newtype NodeType = NodeType Word8
  deriving (Eq, Ord, Num, Storable)

newtype Depth = Depth Word8
  deriving (Eq, Ord, Num, Storable)


{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2
