module Evaluation.StaticExchange where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove      (makeMove)
import           MoveGen.PieceCaptures (staticExchangeCaptures)


{-# INLINE  evaluateCaptureExchange #-}
evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange mv@Move {..} pos =
  evaluateExchange end (makeMove mv pos) - pos.materialScore

{-# INLINE  evaluateExchange #-}
evaluateExchange :: Square -> Position -> Score
evaluateExchange square pos =
  case smallestAttackerMove of
    Just mv -> - (max pos.materialScore
                    (evaluateExchange square (makeMove mv pos)))
    Nothing -> - pos.materialScore
  where
    smallestAttackerMove =
      headMay $ staticExchangeCaptures square pos
