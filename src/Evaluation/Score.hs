module Evaluation.Score where

import           AppPrelude

import           Constants.Boards
import           Models.Piece


newtype Score = Score Int
  deriving (Eq, Ord, Num, Bounded)


{-# INLINE  boardScore #-}
boardScore :: Piece -> Board -> Score
boardScore piece board =
  materialScore piece * Score count
  where
    count = ones board

{-# INLINE  materialScore #-}
materialScore :: Piece -> Score
materialScore = \case
  Pawn -> 100
  Knight -> 310
  Bishop -> 320
  Rook -> 500
  Queen -> 900
  King -> 100_000
