module MoveGen.MakeMove where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Piece
import           Models.Position      (Position (..))
import qualified MoveGen.PieceAttacks as MoveGen


{-# INLINE  makeMove #-}
makeMove :: Move -> Position -> Position
makeMove Move {..} pos =
  switchPlayers
  $ movePiece piece promotion startBoard endBoard
  $ updatePlayerBoards startBoard endBoard pos
  where
    startBoard = toBoard start
    endBoard = toBoard end


{-# INLINE  newPosition #-}
newPosition :: Position -> Position
newPosition = switchPlayers . switchPlayers


{-# INLINE  switchPlayers #-}
switchPlayers :: Position -> Position
switchPlayers pos@Position {..} =
  pos {
    color           = reverseColor color,
    player          = enemy,
    enemy           = player,
    attacked        = MoveGen.allPlayerAttacks pos,
    leapingCheckers = MoveGen.getLeapingCheckers pos,
    sliderCheckers  = MoveGen.getSliderCheckers bishopCheckerRays
                        rookCheckerRays queenCheckerRays pos,
    pinnedPieces    = MoveGen.getPinnedPieces bishopCheckerRays
                        rookCheckerRays sliderRays pos,
    enPassant = toEnum (1 - ones enPassantPinnedPawns) * enPassant

  }
  where
    bishopCheckerRays = MoveGen.getBishopCheckerRays pos
    rookCheckerRays = MoveGen.getRookCheckerRays pos
    queenCheckerRays = bishopCheckerRays .| rookCheckerRays
    sliderRays = MoveGen.getEnemyKingSliderRays pos
    enPassantPinnedPawns
      | enPassant == 0 = 0
      | otherwise = MoveGen.getEnPassantPinnedPawns pos


{-# INLINE  updatePlayerBoards #-}
updatePlayerBoards :: Board -> Board -> Position -> Position
updatePlayerBoards start end pos@Position {..} =
  pos {
    halfMoveClock = (1 - ones (enemy & end)) * (halfMoveClock + 1),
    player = (player ^ start) .| end,
    enemy = enemy .\ end,
    pawns = pawns .\ end,
    knights = knights .\ end,
    bishops = bishops .\ end,
    rooks = rooks .\ end,
    queens = queens .\ end
  }


{-# INLINE  movePiece #-}
movePiece :: Piece -> Maybe Promotion -> Board -> Board -> Position -> Position
movePiece Pawn Nothing start end pos@Position {..} =
  pos {
    halfMoveClock = 0,
    pawns = (pawns ^ (start .| enPassantCapture)) .| end,
    enemy = enemy ^ enPassantCapture,
    enPassant = start <<! 8 & end !>> 8
  }
  where
    ((<<!), (!>>)) = case color of
      White -> ((<<), (>>))
      Black -> ((>>), (<<))
    enPassantCapture = (enPassant & end) !>> 8

movePiece Pawn (Just KnightProm) start end pos@Position {..} =
  pos {
    halfMoveClock = 0,
    pawns = pawns ^ start,
    knights = knights .| end,
    enPassant = 0
  }

movePiece Pawn (Just BishopProm) start end pos@Position {..} =
  pos {
    halfMoveClock = 0,
    pawns = pawns ^ start,
    bishops = bishops .| end,
    enPassant = 0
  }

movePiece Pawn (Just RookProm) start end pos@Position {..} =
  pos {
    halfMoveClock = 0,
    pawns = pawns ^ start,
    rooks = rooks .| end,
    enPassant = 0
  }

movePiece Pawn (Just QueenProm) start end pos@Position {..} =
  pos {
    halfMoveClock = 0,
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
    kingRank = fileMovesVec !! lsb start
