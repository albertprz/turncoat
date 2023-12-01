module MoveGen.MakeMove where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Piece
import           Models.Position    (Position (..))
import           MoveGen.PieceMoves (allEnemyAttacks)


makeMove :: Move -> Position -> Position
makeMove Move {..} pos =
  switchPlayers
  $ movePiece piece promotion startBoard endBoard
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
    queens = queens .\ end
  }

movePiece :: Piece -> Maybe Promotion -> Board -> Board -> Position -> Position
movePiece Pawn Nothing start end pos@Position {..} =
  case color of
    White ->
      pos {
        pawns = (pawns ^ (start .| enPassantCapture)) .| end,
        enemy = enemy ^ enPassantCapture,
        enPassant = start << 8 & end >> 8
      }
      where
        enPassantCapture = (enPassant & end) >> 8
    Black ->
      pos {
        pawns = (pawns ^ (start .| enPassantCapture)) .| end,
        enemy = enemy ^ enPassantCapture,
        enPassant = start >> 8 & end << 8
      }
      where
        enPassantCapture = (enPassant & end) << 8

movePiece Pawn (Just KnightProm) start end pos@Position {..} =
  pos {
    pawns = pawns ^ start,
    knights = knights .| end,
    enPassant = 0
  }

movePiece Pawn (Just BishopProm) start end pos@Position {..} =
  pos {
    pawns = pawns ^ start,
    bishops = bishops .| end,
    enPassant = 0
  }

movePiece Pawn (Just RookProm) start end pos@Position {..} =
  pos {
    pawns = pawns ^ start,
    rooks = rooks .| end,
    enPassant = 0
  }

movePiece Pawn (Just QueenProm) start end pos@Position {..} =
  pos {
    pawns = pawns ^ start,
    queens = queens .| end,
    enPassant = 0
  }

movePiece Knight _ start end pos@Position {..} =
  pos {
    knights = (knights ^ start) .| end,
    enPassant = 0
  }

movePiece Bishop _ start end pos@Position {..} =
  pos {
    bishops = (bishops ^ start) .| end,
    enPassant = 0
  }

movePiece Rook _ start end pos@Position {..} =
  pos {
    rooks = (rooks ^ start) .| end,
    castling = castling .\ start,
    enPassant = 0
  }

movePiece Queen _ start end pos@Position {..} =
  pos {
    queens = (queens ^ start) .| end,
    enPassant = 0
  }

movePiece King _ start end pos@Position {..} =
  pos {
    kings = (kings ^ start) .| end,
    castling = castling .\ kingRank,
    player = (player ^ rookStart) .| rookEnd,
    rooks = (rooks ^ rookStart) .| rookEnd,
    enPassant = 0
  }
  where
    rookStart = shortCastle << 1 .| longCastle >> 2
    rookEnd = shortCastle >> 1 .| longCastle << 1
    shortCastle = (start << 2) & end
    longCastle = (start >> 2) & end
    kingRank = rankMovesVec !! lsb start
