module Evaluation.BoardScore where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Score


{-# INLINE  boardScore #-}
boardScore :: Score -> Vector Score -> Board -> Score
boardScore !pieceTypeScore !pieceSquareTable !board =
  foldlBoard (Score 0) (+) pieceScore board
  where
    pieceScore !n = pieceTypeScore + pieceSquareTable !! n


pawnScore :: Score
pawnScore = 100

knightScore :: Score
knightScore = 310

bishopScore :: Score
bishopScore = 320

rookScore :: Score
rookScore = 500

queenScore :: Score
queenScore = 900
