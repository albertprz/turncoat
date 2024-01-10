{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.StaticExchange where

import           AppPrelude

import           Constants.Boards
import           Evaluation.BoardScore
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove      (makeMove)
import           MoveGen.PieceCaptures (allLegalCaptures)


{-# INLINE  evaluateCaptureExchange #-}
evaluateCaptureExchange :: Position -> Move -> Score
evaluateCaptureExchange !pos mv@Move {..}  =
  pieceScore - evaluateExchange end (makeMove mv pos)
  where
    !pieceScore =
      capturedPieceToScore $ capturedPieceAt end pos


{-# INLINE  evaluateExchange #-}
evaluateExchange :: Square -> Position -> Score
evaluateExchange !square !pos =
  case smallestAttackerMove of
    Nothing -> 0
    Just !mv ->
      max 0 $! (pieceScore - evaluateExchange square
                                              (newPos $! makeMove mv pos))
  where
    newPos x@Position {..} = x
      { enPassant = enPassant
          & (toBoard square << 8 .| toBoard square >> 8)
      }
    pieceScore =
      capturedPieceToScore $ capturedPieceAt square pos

    !smallestAttackerMove =
      headMay $ allLegalCaptures (toBoard square) pos


{-# INLINE  capturedPieceToScore #-}
capturedPieceToScore :: Piece -> Score
capturedPieceToScore = \case
  Pawn -> pawnScore
  Knight -> knightScore
  Bishop -> bishopScore
  Rook -> rookScore
  Queen -> queenScore
