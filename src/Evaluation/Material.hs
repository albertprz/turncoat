module Evaluation.Material (evaluateMaterial, evaluatePlayerMaterial, evaluateCapturedPiece) where

import           AppPrelude

import           Evaluation.Parameters
import           Evaluation.ScoreBreakdown
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           Utils.Board


evaluateMaterial :: (?phase :: Phase) => Position -> Score
evaluateMaterial pos@Position {player, enemy, color} =
    go (evaluatePlayerMaterial pos player color)
  - go (evaluatePlayerMaterial pos enemy (reverseColor color))
  where
    go MaterialBreakdown {..} =
        queensMaterial  + rooksMaterial + bishopsMaterial
      + knightsMaterial + pawnsMaterial
      where
        ScorePair queensMaterial  _ = queens
        ScorePair rooksMaterial   _ = rooks
        ScorePair bishopsMaterial _ = bishops
        ScorePair knightsMaterial _ = knights
        ScorePair pawnsMaterial   _ = pawns


evaluatePlayerMaterial
  :: (?phase :: Phase) => Position -> Board -> Color -> MaterialBreakdown
evaluatePlayerMaterial Position {..} !board = \case
    White -> MaterialBreakdown
          (boardScore queenScore whiteQueenSquareTable   (board & queens))
          (boardScore rookScore whiteRookSquareTable     (board & rooks))
          (boardScore bishopScore whiteBishopSquareTable (board & bishops))
          (boardScore knightScore whiteKnightSquareTable (board & knights))
          (boardScore pawnScore whitePawnSquareTable     (board & pawns))
          (ScorePair 0 (whiteKingSquareTable !!% lsb      (board & kings)))
    Black -> MaterialBreakdown
          (boardScore queenScore blackQueenSquareTable   (board & queens))
          (boardScore rookScore blackRookSquareTable     (board & rooks))
          (boardScore bishopScore blackBishopSquareTable (board & bishops))
          (boardScore knightScore blackKnightSquareTable (board & knights))
          (boardScore pawnScore blackPawnSquareTable     (board & pawns))
          (ScorePair 0 (blackKingSquareTable !!% lsb      (board & kings)))


boardScore :: Score -> Vector Score -> Board -> ScorePair
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard (ScorePair 0 0) foldFn pieceToScores board
  where
    foldFn (ScorePair x y) (ScorePair z t) = ScorePair (x + z) (y + t)
    pieceToScores !n = ScorePair pieceTypeScore (pieceSquareTable !! n)


evaluateCapturedPiece :: Piece -> Score
evaluateCapturedPiece = \case
    Pawn   -> pawnScore
    Knight -> knightScore
    Bishop -> bishopScore
    Rook   -> rookScore
    Queen  -> queenScore
    King   -> 0
