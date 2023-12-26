module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Evaluation.PieceSquareTables
import           Evaluation.Score
import           Models.Piece
import           Models.Position


{-# INLINE  evaluatePosition #-}
evaluatePosition :: Position -> Score
evaluatePosition Position {..} =
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
