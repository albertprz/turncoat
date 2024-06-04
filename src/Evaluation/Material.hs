{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.Material (evaluateMaterial, evaluatePlayerMaterial, evaluateCapturedPiece, pawnScore, knightScore, bishopScore, rookScore, queenScore) where

import           AppPrelude

import           Evaluation.Constants
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           Utils.Board


evaluateMaterial :: Position -> Score
evaluateMaterial pos@Position {..} =
    evaluatePlayerMaterial pos player color
  - evaluatePlayerMaterial pos enemy  (reverseColor color)


evaluatePlayerMaterial :: Position -> Board -> Color -> Score
evaluatePlayerMaterial Position {..} !board  = \case
    White -> boardScore pawnScore whitePawnSquareTable     (board & pawns)
          + boardScore knightScore whiteKnightSquareTable (board & knights)
          + boardScore bishopScore whiteBishopSquareTable (board & bishops)
          + boardScore rookScore whiteRookSquareTable     (board & rooks)
          + boardScore queenScore whiteQueenSquareTable   (board & queens)
          + whiteKingSquareTable !! lsb                   (board & kings)
    Black -> boardScore pawnScore blackPawnSquareTable     (board & pawns)
          + boardScore knightScore blackKnightSquareTable (board & knights)
          + boardScore bishopScore blackBishopSquareTable (board & bishops)
          + boardScore rookScore blackRookSquareTable     (board & rooks)
          + boardScore queenScore blackQueenSquareTable   (board & queens)
          + blackKingSquareTable !! lsb                   (board & kings)



boardScore :: Score -> Vector Score -> Board -> Score
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard 0 (+) pieceToScore board
  where
    pieceToScore !n = pieceTypeScore + pieceSquareTable !! n


evaluateCapturedPiece ::  Color -> Square -> Piece -> Score
evaluateCapturedPiece !color !n = \case
    Pawn   -> pawnScore   + pawnSquareTable   !! idx
    Knight -> knightScore + knightSquareTable !! idx
    Bishop -> bishopScore + bishopSquareTable !! idx
    Rook   -> rookScore   + rookSquareTable   !! idx
    Queen  -> queenScore  + queenSquareTable  !! idx
  where
    !idx = n + 64 * fromIntegral (reverseColor color)


pawnScore :: Score
pawnScore = 100

knightScore :: Score
knightScore = 320

bishopScore :: Score
bishopScore = 330

rookScore :: Score
rookScore = 500

queenScore :: Score
queenScore = 950
