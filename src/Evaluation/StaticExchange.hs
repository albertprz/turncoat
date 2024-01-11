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
import           MoveGen.PieceCaptures (staticExchangeCaptures)


{-# INLINE  evaluateCaptureExchange #-}
evaluateCaptureExchange :: Position -> Move -> Score
evaluateCaptureExchange pos mv@Move {..} =
  promotionScore + capturedPieceScore
                 - evaluateExchange end (makeMove mv pos)
  where
    capturedPieceScore =
      maybe 0 capturedPieceToScore $ maybeCapturedPieceAt end pos
    promotionScore =
      maybe 0 promotionToScore promotion


{-# INLINE  evaluateExchange #-}
evaluateExchange :: Square -> Position -> Score
evaluateExchange square pos =
  maybe 0 (evaluateExchangeHelper square pos) smallestAttackerMove
  where
    smallestAttackerMove =
      headMay $ staticExchangeCaptures square pos


{-# INLINE  evaluateExchangeHelper #-}
evaluateExchangeHelper :: Square -> Position -> Move -> Score
evaluateExchangeHelper square pos mv@Move {..} =
    max 0 $! (relativeScore - evaluateExchange square newPos)
  where
    capturedPieceScore =
      capturedPieceToScore $ capturedPieceAt square pos
    promotionScore =
      maybe 0 promotionToScore promotion
    relativeScore =
      capturedPieceScore + promotionScore
    newPos = updateEnPassant $ makeMove mv pos
    updateEnPassant x@Position {..} =
      let target = toBoard square & enemy & pawns
      in x {
        enPassant = enPassant & (target << 8 .| target >> 8)
      }


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
