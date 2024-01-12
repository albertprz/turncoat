module Evaluation.StaticExchange where

import           AppPrelude

import           Constants.Boards
import           Evaluation.BoardScore
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove      (makeMove)
import           MoveGen.PieceCaptures (staticExchangeCaptures)


{-# INLINE  evaluateCaptureExchange #-}
evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange mv@Move {..} pos =
  side * finalPos.materialScore - pos.materialScore
  where
    !side | finalPos.color == pos.color =   1
          | otherwise                  = - 1
    !finalPos = iterateMaybe (toQuietPosition end)
                             (makeMove mv pos)

{-# INLINE  toQuietPosition #-}
toQuietPosition :: Square -> Position -> Maybe Position
toQuietPosition square pos =
  (`makeMove` pos) <$> smallestAttackerMove
  where
    smallestAttackerMove =
      headMay $ staticExchangeCaptures square pos


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
      evaluateCapturedPiece $ capturedPieceAt square pos
    promotionScore =
      maybe 0 evaluatePromotion promotion
    relativeScore =
      capturedPieceScore + promotionScore
    newPos = updateEnPassant $ makeMove mv pos
    updateEnPassant x@Position {..} =
      let target = toBoard square & enemy & pawns
      in x {
        enPassant = enPassant & (target << 8 .| target >> 8)
      }
