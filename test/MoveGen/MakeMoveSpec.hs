module MoveGen.MakeMoveSpec where

import           AppPrelude

import           Test.QuickCheck.Property

import           Constants.Boards
import           Data.List.NonEmpty       (nonEmpty)
import           Evaluation.Evaluation
import           Models.Position
import           MoveGen.MakeMove         (makeMove)
import           MoveGen.PieceMoves
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = do

  it "Material score is calculated incrementally on makeMove" $
    quickCheckWith stdArgs {maxSuccess = 1_000 } $ forAll movesGen
    \idxs ->
      let newPos = applyMoves idxs startPosition
      in newPos.materialScore == evaluateMaterial newPos



applyMoves :: [Int] -> Position -> Position
applyMoves = flip $ foldl' $ flip applyMoveIdx

applyMoveIdx :: Int -> Position -> Position
applyMoveIdx idx pos =
  maybe pos (`makeMove` pos) chosenMove
  where
    chosenMove = (`index` (idx % length moves)) =<< moves
    moves = map toList $ nonEmpty $ allMoves pos


movesGen :: Gen [Int]
movesGen = vectorOf 100 $ chooseInt (0, 40)
