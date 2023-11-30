module Models.Position where


import           Constants.Boards
import           Models.Piece     (Color (..))


data Position = Position {
  color     :: Color,
  attacked  :: Board,
  castling  :: Board,
  enPassant :: Board,
  player    :: Board,
  enemy     :: Board,
  pawns     :: Board,
  rooks     :: Board,
  knights   :: Board,
  bishops   :: Board,
  queens    :: Board,
  kings     :: Board
}

startPosition :: Position
startPosition = Position {
  color = White
  , attacked = 0
  , castling = (rank_1 .| rank_8) & (file_A .| file_E .| file_H)
  , enPassant = 0
  , player = rank_1 .| rank_2
  , enemy = rank_7 .| rank_8
  , pawns = rank_2 .| rank_7
  , rooks = (rank_1 .| rank_8) & (file_A .| file_H)
  , knights = (rank_1 .| rank_8) & (file_B .| file_G)
  , bishops = (rank_1 .| rank_8) & (file_C .| file_F)
  , queens = (rank_1 .| rank_8) & file_D
  , kings = (rank_1 .| rank_8) & file_E
}
