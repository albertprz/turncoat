{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Evaluation.Material where

import           AppPrelude

import           Constants.Boards
import           Evaluation.PieceSquareTables
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score


{-# INLINE  evaluateMaterial #-}
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


{-# INLINE  boardScore #-}
boardScore :: Score -> Vector Score -> Board -> Score
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard (Score 0) (+) pieceToScore board
  where
    pieceToScore !n = pieceTypeScore + pieceSquareTable !! n


{-# INLINE  evaluateCapturedPiece #-}
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
knightScore = 325

bishopScore :: Score
bishopScore = 335

rookScore :: Score
rookScore = 520

queenScore :: Score
queenScore = 1000
