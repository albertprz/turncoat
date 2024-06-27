module MoveGen.MakeMoveSpec where

import           AppPrelude

import           Data.List.NonEmpty (nonEmpty)
import           Models.Position
import           MoveGen.MakeMove   (makeMove)
import           Search.Perft
import           Test.QuickCheck



applyMoves :: [Int] -> Position -> Position
applyMoves = flip $ foldl' $ flip applyMoveIdx


applyMoveIdx :: Int -> Position -> Position
applyMoveIdx idx pos =
  maybe pos (`makeMove` pos) chosenMove
  where
    chosenMove = (`index` (idx `mod` length moves)) =<< moves
    moves = map toList $ nonEmpty $ allMoves pos


movesGen :: Gen [Int]
movesGen = vectorOf 100 $ chooseInt (0, 40)
