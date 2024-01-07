module Models.Score where

import           AppPrelude
import           GHC.Word        (Word16)
import           Test.QuickCheck (Arbitrary (..), chooseEnum)

newtype Score = Score Word16
  deriving (Eq, Ord, Num, Enum, Bounded, Storable, Arbitrary, Show)

newtype NodeType = NodeType Word8
  deriving (Eq, Ord, Num, Enum, Storable, Show)

instance Arbitrary NodeType where
  arbitrary = chooseEnum (PV, All)

newtype Depth = Depth Word8
  deriving (Eq, Ord, Num, Storable, Arbitrary, Show)


{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2
