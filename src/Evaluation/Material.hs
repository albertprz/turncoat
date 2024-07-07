module Evaluation.Material (evaluatePlayerMaterial, evaluateCapturedPiece) where

import           AppPrelude

import           Evaluation.Parameters
import           Evaluation.ScoreBreakdown
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           Utils.Board


evaluatePlayerMaterial
  :: (?phase :: Phase) => Position -> Board -> Color -> MaterialBreakdown
evaluatePlayerMaterial Position {..} !board = \case
    White -> MaterialBreakdown
          (boardMaterialScore queenScore                 (board & queens))
          (boardMaterialScore rookScore                  (board & rooks))
          (boardScore bishopScore whiteBishopSquareTable (board & bishops))
          (boardScore knightScore whiteKnightSquareTable (board & knights))
          (boardMaterialScore pawnScore                  (board & pawns))
          (ScorePair 0 (whiteKingSquareTable !!% lsb     (board & kings)))
    Black -> MaterialBreakdown
          (boardMaterialScore queenScore                 (board & queens))
          (boardMaterialScore rookScore                  (board & rooks))
          (boardScore bishopScore blackBishopSquareTable (board & bishops))
          (boardScore knightScore blackKnightSquareTable (board & knights))
          (boardMaterialScore pawnScore                  (board & pawns))
          (ScorePair 0 (blackKingSquareTable !!% lsb     (board & kings)))


{-# INLINE  boardMaterialScore #-}
boardMaterialScore :: Score -> Board -> ScorePair
boardMaterialScore !pieceTypeScore !board =
  ScorePair materialScore 0
  where
    materialScore = pieceTypeScore * fromIntegral (popCount board)


{-# INLINE  boardScore #-}
boardScore :: (?phase::Phase) => Score -> Vector ScorePair -> Word64 -> ScorePair
boardScore !pieceTypeScore !squareTable !board =
  ScorePair materialScore squareTableScore
  where
    squareTableScore = foldlBoard 0 (+) (squareTable !!%) board
    materialScore    = pieceTypeScore * fromIntegral (popCount board)


{-# INLINE  evaluateCapturedPiece #-}
evaluateCapturedPiece :: (?phase :: Phase) => Move -> Position -> Score
evaluateCapturedPiece Move {..} pos =
  evaluatePromotion promotion
  + maybe 0 evaluatePiece (maybeCapturedPieceAt end pos)


{-# INLINE  evaluatePromotion #-}
evaluatePromotion :: (?phase :: Phase) => Promotion -> Score
evaluatePromotion = \case
  KnightProm -> knightScore
  BishopProm -> bishopScore
  RookProm   -> rookScore
  QueenProm  -> queenScore
  NoProm     -> 0


{-# INLINE  evaluatePiece #-}
evaluatePiece :: (?phase :: Phase) => Piece -> Score
evaluatePiece = \case
  Pawn   -> pawnScore
  Knight -> knightScore
  Bishop -> bishopScore
  Rook   -> rookScore
  Queen  -> queenScore
  King   -> 0
