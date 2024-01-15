module Models.Score where

import           AppPrelude
import           Data.Int        (Int16)
import           Test.QuickCheck (Arbitrary (..), elements)

newtype Score = Score Int16
  deriving (Eq, Ord, Num, Enum, Bounded, Storable, Arbitrary, Show)

newtype NodeType = NodeType Word8
  deriving (Eq, Ord, Num, Enum, Storable, Show)

instance Arbitrary NodeType where
  arbitrary = elements [PV, Cut, All]

newtype Depth = Depth Word8
  deriving (Eq, Ord, Num, Enum, Storable, Arbitrary, Show)

newtype Ply = Ply Word8
  deriving (Eq, Ord, Num, Enum, Real, Integral)

{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2


{-# INLINE  getNodeType #-}
getNodeType :: Score -> Score -> Score -> NodeType
getNodeType !alpha !beta !score
  | score >= beta  = Cut
  | score > alpha = PV
  | otherwise     = All
