module MoveGen.PositionQueries (isDefeat, isNotWinnable, isDraw, isEndgame, isKingInCheck, hasSingleMove) where

import           AppPrelude

import           Models.Position
import           Search.Perft
import           Utils.Board


isEndgame :: Position -> Bool
isEndgame Position {..} =
  popCount (player & majorAndMinorPieces) == 0
  where
    majorAndMinorPieces = queens .| rooks .| bishops .| knights


isKingInCheck :: Position -> Bool
isKingInCheck Position {..} =
  sliderCheckers .| leapingCheckers /= 0


hasSingleMove :: Position -> Bool
hasSingleMove = hasOne . allMoves
  where
  hasOne [_] = True
  hasOne _   = False


isDefeat :: Position -> Bool
isDefeat = isCheckmate


isNotWinnable :: Position -> Bool
isNotWinnable Position {..} =
  player & (pawns .| majorPieces) == 0
  && popCount (player & minorPieces) <= 1
  && enemy & pawns /= 0
  where
    minorPieces = bishops .| knights
    majorPieces = queens  .| rooks


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

   allPieces == majorPieces .| minorPieces &&
     forBoth queenVsRookAndMinor

  || allPieces == majorPieces &&
    forBoth queenVsRooks

  || allPieces == rooks &&
    (popCount (player & rooks) == 1
    && popCount (enemy & rooks) == 1)

  || allPieces == rooks .| minorPieces &&
    (forBoth rooksVsRookAndMinor
    || forBoth rooksVsRookBishopAndKnight
    || forBoth rookVsMinor
    || forBoth rookAndMinorVsRook
    || forBoth bishopAndKnightVsRook)

  || allPieces == minorPieces &&

    ((playerMinorPiecesCount <= 1
        && enemyMinorPiecesCount <= 1)

    || (popCount bishops == 3 && knights == 0)

    || forBoth knightsVsMinor

    || forBoth bishopAndKnightVsMinor)

  where
    knightsVsMinor !board !enemyBoard =
      popCount (board & knights)              <= 2
        && board & bishops                     == 0
        && popCount (enemyBoard & minorPieces) <= 1

    bishopAndKnightVsMinor !board !enemyBoard =
      popCount (board & bishops)              == 1
        && popCount (board & knights)          == 1
        && popCount (enemyBoard & minorPieces) == 1

    queenVsRooks !board !enemyBoard =
      board & rooks                     == 0
        && popCount (board & queens)     == 1
        && enemyBoard & queens           == 0
        && popCount (enemyBoard & rooks) == 2

    rookVsMinor !board !enemyBoard =
      board & minorPieces                    == 0
       && popCount (board & rooks)            == 1
       && enemyBoard & rooks                  == 0
       && popCount (enemyBoard & minorPieces) == 1

    rookAndMinorVsRook !board !enemyBoard =
      popCount (board & rooks)         == 1
      && popCount (board & minorPieces) == 1
      && popCount (enemyBoard & rooks)  == 1
      && enemyBoard & minorPieces       == 0

    bishopAndKnightVsRook !board !enemyBoard =
      board & rooks                == 0
      && popCount (board & bishops) == 1
      && popCount (board & knights) == 1
      && enemyBoard & minorPieces   == 0
      && popCount (board & rooks)   == 1

    queenVsRookAndMinor !board !enemyBoard =
      board & (rooks .| minorPieces)        == 0
      && popCount (board & queens)           == 1
      && enemyBoard & queens                 == 0
      && popCount (enemyBoard & rooks)       == 1
      && popCount (enemyBoard & minorPieces) == 1

    rooksVsRookAndMinor !board !enemyBoard =
      board & minorPieces        == 0
      && popCount (board & rooks)           == 2
      && popCount (enemyBoard & rooks)       == 1
      && popCount (enemyBoard & minorPieces) == 1

    rooksVsRookBishopAndKnight !board !enemyBoard =
      board & minorPieces               == 0
      && popCount (board & rooks)        == 2
      && popCount (enemyBoard & rooks)   == 1
      && popCount (enemyBoard & knights) == 1
      && popCount (enemyBoard & rooks)   == 1


    !playerMinorPiecesCount = popCount (player & minorPieces)
    !enemyMinorPiecesCount  = popCount (enemy  & minorPieces)
    !allPieces              = (player .| enemy) .\ kings
    !minorPieces            = knights .| bishops
    !majorPieces            = rooks   .| queens
    forBoth fn = fn player enemy || fn enemy player
