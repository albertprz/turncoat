module Search.Search where

import           AppPrelude

import           Evaluation.Evaluation (evaluatePosition)
import           Evaluation.Score
import           Models.Move
import           Models.Position       (Position)
import           MoveGen.MakeMove      (playMove)
import           MoveGen.PieceMoves

import           Utils.Ord


{-# INLINE  getBestMove #-}
getBestMove :: Int -> Position -> Maybe Move
getBestMove depth pos =
  maximumByOrd (moveScore depth pos) (allLegalMoves pos)

{-# INLINE  negamax #-}
negamax :: Int -> Position -> Score
negamax depth pos
  | depth == 0 = evaluatePosition pos
  | otherwise = fromMaybe minBound
    $ maximumMay
    $ map (moveScore depth pos)
    $ allLegalMoves pos

{-# INLINE  moveScore #-}
moveScore :: Int -> Position -> Move -> Score
moveScore depth pos move =
  - negamax (depth - 1) (playMove move pos)
