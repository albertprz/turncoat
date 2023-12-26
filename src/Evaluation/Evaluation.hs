module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Evaluation.Score
import           Models.Piece
import           Models.Position

evaluatePosition :: Position -> Score
evaluatePosition Position {..} =
  boardScoreDiff Pawn pawns
  + boardScoreDiff Knight knights
  + boardScoreDiff Bishop bishops
  + boardScoreDiff Rook rooks
  + boardScoreDiff Queen queens
  + boardScoreDiff King kings
  where
    boardScoreDiff piece board =
      boardScore piece (board & player) - boardScore piece (board & enemy)
