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
evaluateCaptureExchange !pos mv@Move {..} =
  promotionScore + pieceScore - evaluateExchange end (makeMove mv pos)
  where
    pieceScore =
      maybe 0 capturedPieceToScore $ maybeCapturedPieceAt end pos
    promotionScore =
      maybe 0 promotionToScore promotion

{-# INLINE  evaluateExchange #-}
evaluateExchange :: Square -> Position -> Score
evaluateExchange !square !pos =
  case smallestAttackerMove of
    Nothing -> 0
    Just !mv ->
      max 0 $! (promotionScore mv + pieceScore
                - evaluateExchange square (newPos $! makeMove mv pos))
  where
    newPos x@Position {..} =
      let target = toBoard square & enemy & pawns
      in x {
        enPassant = enPassant & (target << 8 .| target >> 8)
      }
    pieceScore =
      capturedPieceToScore $ capturedPieceAt square pos

    promotionScore Move {..} =
      maybe 0 promotionToScore promotion

    smallestAttackerMove =
      headMay $ allLegalCaptures (toBoard square) pos


{-# INLINE  capturedPieceToScore #-}
capturedPieceToScore :: Piece -> Score
capturedPieceToScore = \case
  Pawn -> pawnScore
  Knight -> knightScore
  Bishop -> bishopScore
  Rook -> rookScore
  Queen -> queenScore

{-# INLINE  promotionToScore #-}
promotionToScore :: Promotion -> Score
promotionToScore prom = score - pawnScore
  where
    score = case prom of
      KnightProm -> knightScore
      BishopProm -> bishopScore
      RookProm   -> rookScore
      QueenProm  -> queenScore
