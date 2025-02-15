module Search.PerftSpec where

import           AppPrelude

import           Control.Error.Util (hush)
import           Data.Maybe         (fromJust)
import           Test.Hspec

import           Bookhound.Parser
import           Models.Position
import           Parsers.Position
import           Search.Perft

spec :: Spec
spec = do

  describe "Perft tests" do

    it "Initial Position" do
      perft 6 startPosition `shouldBe` 119_060_324

    it "Kiwipete" do
      perft 5 kiwipete `shouldBe` 193_690_690

    it "Extra Position 1" do
      perft 7 extraPos1 `shouldBe` 178_633_661

    it "Extra Position 2" do
      perft 6 extraPos2 `shouldBe` 706_045_033

    it "Extra Position 3" do
      perft 5 extraPos3 `shouldBe` 89_941_194

    it "Extra Position 4" do
      perft 5 extraPos4 `shouldBe` 164_075_551

    it "Extra Position 5" do
      perft 5 extraPos5 `shouldBe` 73_007_159



kiwipete :: Position
kiwipete = fenPosition
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"

extraPos1 :: Position
extraPos1 = fenPosition
  "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1"

extraPos2 :: Position
extraPos2 = fenPosition
  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"

extraPos3 :: Position
extraPos3 = fenPosition
  "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 0 1"

extraPos4 :: Position
extraPos4 = fenPosition
  "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 1"

extraPos5 :: Position
extraPos5 = fenPosition
  "1k1r2r1/1pp2p1p/np1p1pb1/3P2q1/1PPN4/P6Q/4BPPP/3RR1K1 b - - 0 1"


fenPosition :: Text -> Position
fenPosition = fromJust . hush . runParser positionFenParser
