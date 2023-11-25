module MoveGen.PieceMoves where

import           Models.Board
import           Models.Piece        (Color (..))
import           MoveGen.PieceBoards


pawnMoves :: Color -> Board -> Board
pawnMoves color pos = case color of
  White -> pos << 8 .| rank_2 & pos << 16
  Black -> pos >> 8 .| rank_7 & pos >> 16

pawnCaptures :: Color -> Board -> Board
pawnCaptures color pos = case color of
  White -> (pos .\ file_A) << 9 .| (pos .\ file_H) << 7
  Black -> (pos .\ file_A) >> 7 .| (pos .\ file_H) >> 9
