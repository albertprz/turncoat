module MoveGen.PositionQueries (isDefeat, isDraw, isEndgame, isKingInCheck, hasSingleMove) where

import           AppPrelude

import           Models.Position
import           Search.Perft
import           Utils.Board


isEndgame :: Position -> Bool
isEndgame Position {..} =
  popCount (player & allPieces) <= 1
  where
    allPieces = bishops .| knights .| rooks .| queens


isDefeat :: Position -> Bool
isDefeat = isCheckmate


isDraw :: Position -> Bool
isDraw pos =
    isDrawByHalfMoves pos
  || isDrawByMaterial pos
  || isStalemate pos
  || isDrawByRepetition pos


isCheckmate :: Position -> Bool
isCheckmate pos = null (allMoves pos) && isKingInCheck pos


isStalemate :: Position -> Bool
isStalemate pos = null (allMoves pos) && not (isKingInCheck pos)


isDrawByHalfMoves :: Position -> Bool
isDrawByHalfMoves Position {halfMoveClock} =
  halfMoveClock == 50


isDrawByRepetition :: Position -> Bool
isDrawByRepetition pos =
  isRepeatedPosition (getZobristKey pos) pos


isDrawByMaterial :: Position -> Bool
isDrawByMaterial Position {..} =
  allPieces == kings .| minorPieces &&

    (playerMinorPiecesCount <= 1
     && enemyMinorPiecesCount <= 1)

   || (popCount bishops == 3
      && knights == 0)

   || (popCount (player & knights) <= 2
      && player & bishops == 0
      && enemyMinorPiecesCount <= 1)

   || (popCount (enemy & knights) <= 2
      && enemy & bishops == 0
      && playerMinorPiecesCount <= 1)

   || (popCount (player & bishops) == 1
      && popCount (player & knights) == 1
      && enemyMinorPiecesCount == 1)

   || (popCount (enemy & bishops) == 1
      && popCount (enemy & knights) == 1
      && playerMinorPiecesCount == 1)
  where
    !playerMinorPiecesCount = popCount (player & minorPieces)
    !enemyMinorPiecesCount  = popCount (enemy  & minorPieces)
    !allPieces              = player  .| enemy
    !minorPieces            = knights .| bishops


isKingInCheck :: Position -> Bool
isKingInCheck Position {..} =
  sliderCheckers .| leapingCheckers /= 0


hasSingleMove :: Position -> Bool
hasSingleMove = hasOne . allMoves
  where
  hasOne [_] = True
  hasOne _   = False
