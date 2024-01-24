module MoveGen.MoveQueries where

import           AppPrelude

import           Constants.Boards
import           Data.Bits            (Bits (testBit))
import           Data.Composition
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceAttacks


{-# INLINE  isCheckMove #-}
isCheckMove :: Move -> Position -> Bool
isCheckMove move pos =
  isKingInCheck $ makeMove move pos


{-# INLINE  isCapture #-}
isCapture :: Move -> Position -> Bool
isCapture Move {..} Position {..} =
  isJust promotion
  || testBit (enemy .| enPassant) end


{-# INLINE  isCheckOrCapture #-}
isCheckOrCapture :: Move -> Position -> Bool
isCheckOrCapture move pos =
  isCheckMove move pos
    || isCapture move pos


{-# INLINE  isQuietMove #-}
isQuietMove :: Move -> Position -> Bool
isQuietMove = not .: isCapture


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
