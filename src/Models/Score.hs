module Models.Score where

import           AppPrelude

import           Data.Int        (Int16)
import           Test.QuickCheck (Arbitrary (..), elements)


type Score = Int16
type Depth = Word8
type Ply   = Word8


newtype NodeType = NodeType Word8
  deriving (Eq, Ord, Num, Storable, Show)

instance Arbitrary NodeType where
  arbitrary = elements [PV, Cut, All]


{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2


getNodeType :: Score -> Score -> Score -> NodeType
getNodeType !alpha !beta !score
  | score >= beta  = Cut
  | score > alpha = PV
  | otherwise     = All
