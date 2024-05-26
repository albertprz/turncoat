{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.Material where

import           AppPrelude

import           Constants.Boards
import           Evaluation.PieceTables
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score


evaluateMaterial :: Position -> Score
evaluateMaterial Position {..} =
  go color player - go (reverseColor color) enemy
  where
  go White !board =
    boardScore pawnScore whitePawnSquareTable (board & pawns)
    + boardScore knightScore whiteKnightSquareTable (board & knights)
    + boardScore bishopScore whiteBishopSquareTable (board & bishops)
    + boardScore rookScore whiteRookSquareTable (board & rooks)
    + boardScore queenScore whiteQueenSquareTable (board & queens)
    + whiteKingSquareTable !! lsb (board & kings)
  go Black !board =
    boardScore pawnScore blackPawnSquareTable (board & pawns)
    + boardScore knightScore blackKnightSquareTable (board & knights)
    + boardScore bishopScore blackBishopSquareTable (board & bishops)
    + boardScore rookScore blackRookSquareTable (board & rooks)
    + boardScore queenScore blackQueenSquareTable (board & queens)
    + blackKingSquareTable !! lsb (board & kings)


boardScore :: Score -> Vector Score -> Board -> Score
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard 0 (+) pieceToScore board
  where
    pieceToScore !n = pieceTypeScore + pieceSquareTable !! n


evaluateCapturedPiece ::  Color -> Square -> Piece -> Score
evaluateCapturedPiece !color !n !piece =
  case piece of
    Pawn   -> pawnScore + pawnSquareTable !! idx
    Knight -> knightScore + knightSquareTable !! idx
    Bishop -> bishopScore + bishopSquareTable !! idx
    Rook   -> rookScore + rookSquareTable !! idx
    Queen  -> queenScore + queenSquareTable !! idx
  where
    idx = n + 64 * fromIntegral (reverseColor color)


pawnScore :: Score
pawnScore = 100

knightScore :: Score
knightScore = 330

bishopScore :: Score
bishopScore = 340

rookScore :: Score
rookScore = 510

queenScore :: Score
queenScore = 1000
