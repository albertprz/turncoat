module Models.TranspositionTableSpec where

import           AppPrelude

import           Test.QuickCheck.Property

import           Models.Move
import           Models.TranspositionTable
import           Models.TranspositionTable (emptyTEntry)
import           Test.Hspec                (Spec, it, shouldBe)
import           Test.Hspec.QuickCheck     (prop)
import           Test.QuickCheck           (Args (maxSuccess), quickCheckWith,
                                            stdArgs)


spec :: Spec
spec = do

  it "Encode / decode Table entries" $
    quickCheckWith stdArgs {maxSuccess = 1_000 }
    \x -> (decodeTEntry . encodeTEntry) x
         ===
         Just x

  it "Decode empty Table entry" $
     decodeTEntry emptyTEntry `shouldBe` Nothing
