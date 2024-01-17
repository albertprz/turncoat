module MoveGen.MoveQueries where

import           AppPrelude

import           Constants.Boards
import           Data.Bits            (Bits (testBit))
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks


{-# INLINE  isKingInCheck #-}
isKingInCheck :: Position -> Bool
isKingInCheck Position {..} =
  sliderCheckers .| leapingCheckers /= 0


{-# INLINE  isEnemyKingInCheck #-}
isEnemyKingInCheck :: Position -> Bool
isEnemyKingInCheck pos@Position {..} =
  player & potentialCheckers /= 0
  where
    potentialCheckers =
      pawns & pawnAttacks (reverseColor color) (enemy&kings)
      .| knights & knightAttacks (lsb (enemy&kings))
      .| kings & kingAttacks (lsb (enemy&kings))
      .| bishopCheckerRays & bishops
      .| rookCheckerRays   & rooks
      .| queenCheckerRays  & queens
    bishopCheckerRays = getBishopCheckerRays pos
    rookCheckerRays = getRookCheckerRays pos
    queenCheckerRays = bishopCheckerRays .| rookCheckerRays

{-# INLINE  isQuietMove #-}
isQuietMove :: Move -> Position -> Bool
isQuietMove Move {..} Position {..} =
  isNothing promotion
    && not (testBit (enemy .| enPassant) end)


{-# INLINE  isLegalQuietMove #-}
isLegalQuietMove :: Move -> Position -> Bool
isLegalQuietMove mv@Move {..} pos@Position {..} =
  testBit player start
    && not (testBit allPieces end)
    && not (piece == King && abs (start - end) == 2)
    && isPieceAt piece start pos
    && not (isEnemyKingInCheck $ quietMakeMove mv pos)
  where
    allPieces = enemy .| player


{-# INLINE  quietMakeMove #-}
quietMakeMove :: Move -> Position -> Position
quietMakeMove Move {..} pos = newPos {
    player = newPos.enemy,
    enemy = newPos.player,
    color = reverseColor newPos.color
  }
  where
    newPos = quietMovePiece piece startBoard endBoard pos
    startBoard = toBoard start
    endBoard = toBoard end


{-# INLINE  quietMovePiece #-}
quietMovePiece :: Piece -> Board -> Board -> Position -> Position
quietMovePiece Pawn start end pos@Position {..} =
  pos {
    pawns = (pawns ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Knight start end pos@Position {..} =
  pos {
    knights = (knights ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Bishop start end pos@Position {..} =
  pos {
    bishops = (bishops ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Rook start end pos@Position {..} =
  pos {
    rooks = (rooks ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Queen start end pos@Position {..} =
  pos {
    queens = (queens ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece King start end pos@Position {..} =
  pos {
    kings = (kings ^ start) .| end,
    player = (player ^ start) .| end
  }
