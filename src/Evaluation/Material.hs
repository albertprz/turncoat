{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.Material (evaluateMaterial, evaluatePlayerMaterial, evaluateCapturedPiece) where

import           AppPrelude

import           Evaluation.Parameters
import           Evaluation.ScoreBreakdown
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           Utils.Board


evaluateMaterial :: Position -> Score
evaluateMaterial pos@Position {..} =
    evalScore (evaluatePlayerMaterial pos player color)
  - evalScore (evaluatePlayerMaterial pos enemy (reverseColor color))


evaluatePlayerMaterial :: Position -> Board -> Color -> MaterialBreakdown
evaluatePlayerMaterial Position {..} !board  = \case
    White -> MaterialBreakdown
          (boardScore queenScore whiteQueenSquareTable   (board & queens))
          (boardScore rookScore whiteRookSquareTable     (board & rooks))
          (boardScore bishopScore whiteBishopSquareTable (board & bishops))
          (boardScore knightScore whiteKnightSquareTable (board & knights))
          (boardScore pawnScore whitePawnSquareTable     (board & pawns))
          (ScorePair 0 (whiteKingSquareTable !! lsb      (board & kings)))
    Black -> MaterialBreakdown
          (boardScore queenScore blackQueenSquareTable   (board & queens))
          (boardScore rookScore blackRookSquareTable     (board & rooks))
          (boardScore bishopScore blackBishopSquareTable (board & bishops))
          (boardScore knightScore blackKnightSquareTable (board & knights))
          (boardScore pawnScore blackPawnSquareTable     (board & pawns))
          (ScorePair 0 (blackKingSquareTable !! lsb      (board & kings)))


boardScore :: Score -> Vector Score -> Board -> ScorePair
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard (ScorePair 0 0) foldFn pieceToScores board
  where
    foldFn (ScorePair x y) (ScorePair z t) = ScorePair (x + z) (y + t)
    pieceToScores !n = ScorePair pieceTypeScore (pieceSquareTable !! n)


evaluateCapturedPiece ::  Color -> Square -> Piece -> Score
evaluateCapturedPiece !color !n = \case
    Pawn   -> pawnScore   + pawnSquareTable   !! idx
    Knight -> knightScore + knightSquareTable !! idx
    Bishop -> bishopScore + bishopSquareTable !! idx
    Rook   -> rookScore   + rookSquareTable   !! idx
    Queen  -> queenScore  + queenSquareTable  !! idx
  where
    !idx = n + 64 * fromIntegral (reverseColor color)
