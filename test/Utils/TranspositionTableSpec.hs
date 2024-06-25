module Utils.TranspositionTableSpec where

import           AppPrelude

import           Test.QuickCheck.Property

import           Test.Hspec               (Spec, it, shouldBe)
import           Test.QuickCheck          (Args (maxSuccess), quickCheckWith,
                                           stdArgs)
import           Utils.TranspositionTable


spec :: Spec
spec = do

  it "Encode then decode Table entries" $
    quickCheckWith stdArgs {maxSuccess = 1_000 }
    \tEntry -> (decodeTEntry . encodeTEntry) tEntry
         === Just tEntry

  it "Decode empty Table entry" $
     decodeTEntry emptyTEntry `shouldBe` Nothing
