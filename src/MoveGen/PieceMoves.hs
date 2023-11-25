module MoveGen.PieceMoves where

import           Models.Board (Board, (&), (.|), (<<), (>>))
import           Models.Piece (Color (..))

pawnMoves :: Color -> Board -> Board
pawnMoves color pos = case color of
  White -> pos << 8 .| rank_2 & pos << 16
  Black -> pos >> 8 .| rank_7 & pos >> 16


rank_2 :: Board
rank_2 = 1 << 2

rank_7 :: Board
rank_7 = 1 << 7
