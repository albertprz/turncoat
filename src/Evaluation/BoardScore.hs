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


{-# INLINE  pawnScore #-}
pawnScore :: Score
pawnScore = 100

{-# INLINE  knightScore #-}
knightScore :: Score
knightScore = 310

{-# INLINE  bishopScore #-}
bishopScore :: Score
bishopScore = 320

{-# INLINE  rookScore #-}
rookScore :: Score
rookScore = 500

{-# INLINE  queenScore #-}
queenScore :: Score
queenScore = 900
