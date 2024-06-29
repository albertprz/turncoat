module MoveGen.PositionQueries (isEndgame, isKingInCheck, isEnemyKingInCheck, hasSingleMove) where

import           AppPrelude

import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks
import           Search.Perft
import           Utils.Board


isEndgame :: Position -> Bool
isEndgame Position {..} =
      popCount (player & allMinorPieces) < 3
    || popCount (enemy  & allMinorPieces) < 3
  where
    allMinorPieces = bishops .| knights .| rooks .| queens


hasSingleMove :: Position -> Bool
hasSingleMove = hasOne . allMoves
  where
  hasOne [_] = True
  hasOne _   = False


isKingInCheck :: Position -> Bool
isKingInCheck Position {..} =
  sliderCheckers .| leapingCheckers /= 0


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
