module MoveGen.MakeMove where

import AppPrelude

import Models.Piece
import Models.Board
import Models.Position (Position(..))
import MoveGen.PieceMoves (allEnemyAttacks)


makeMove :: Move -> Position -> Position
makeMove (piece, start, end) pos =
  switchPlayers
  $ movePiece piece startBoard endBoard
  $ updatePlayerBoards startBoard endBoard pos
  where
    startBoard = toBoard start
    endBoard = toBoard end

switchPlayers :: Position -> Position
switchPlayers pos@Position {..} =
  pos {
    color = reverseColor color,
    attacked = allEnemyAttacks pos,
    player = enemy,
    enemy = player
  }

updatePlayerBoards :: Board -> Board -> Position -> Position
updatePlayerBoards start end pos@Position {..} =
  pos {
    player = (player ^ start) .| end,
    enemy = enemy .\ end,
    pawns = pawns .\ end,
    knights = knights .\ end,
    bishops = bishops .\ end,
    rooks = rooks .\ end,
    queens = rooks .\ end
  }

movePiece :: Piece -> Board -> Board -> Position -> Position
movePiece Pawn start end pos@Position {..} =
  case color of
    White ->
      pos {
        pawns = (pawns ^ (start .| enPassantCapture)) .| end,
        enPassant = start << 8 & end >> 8,
        enemy = enemy ^ enPassantCapture
      }
      where
        enPassantCapture = (enPassant & end) >> 8
    Black ->
      pos {
        pawns = (pawns ^ (start .| enPassantCapture)) .| end,
        enPassant = start >> 8 & end << 8,
        enemy = enemy ^ enPassantCapture
      }
      where
        enPassantCapture = (enPassant & end) << 8

movePiece Knight start end pos@Position {..} =
  pos {
    knights = (knights ^ start) .| end
  }

movePiece Bishop start end pos@Position {..} =
  pos {
    bishops = (bishops ^ start) .| end
  }

movePiece Rook start end pos@Position {..} =
  pos {
    rooks = (rooks ^ start) .| end
  }

movePiece Queen start end pos@Position {..} =
  pos {
    queens = (queens ^ start) .| end
  }
