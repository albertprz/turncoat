module MoveGen.PieceMoves where

import           AppPrelude

import           Data.Bits.Extras    (Ranked (lsb), msb)
import           Models.Board
import           Models.Piece
import           Models.Position
import           MoveGen.PieceBoards

type Move = (Color, Int, Board)

kingInCheck :: Position -> Bool
kingInCheck pos@Position {..} =
  player&kings & allEnemyAttacks pos /= 0

allPlayerAttacks :: Position -> Board
allPlayerAttacks pos@Position {..} =
  allAttacks player enemy color pos

allEnemyAttacks :: Position -> Board
allEnemyAttacks pos@Position {..} =
  allAttacks enemy player (reverseColor color) pos

allAttacks :: Board -> Board -> Color -> Position -> Board
allAttacks player enemy color
  (Position {pawns, knights, bishops, rooks, queens, kings}) =
     pawnAttacks  color                     (player&pawns)
  .| foldMapBoard knightAttacks             (player&knights)
  .| foldMapBoard (bishopAttacks allPieces) (player&bishops)
  .| foldMapBoard (rookAttacks allPieces)   (player&rooks)
  .| foldMapBoard (queenAttacks allPieces)  (player&queens)
  .| kingAttacks                       (lsb (player&kings))
  where
    allPieces = player .| enemy

-- allMoves :: Board -> Board -> Color -> Position -> [Move]

pawnMoves :: Board -> Board -> Color -> Board -> Board
pawnMoves allPieces player color board =
  (pawnAdvances allPieces color board
  .| pawnAttacks color board)
  .\ player

knightMoves :: Board -> Square -> Board
knightMoves player n =
  knightAttacks n .\ player

kingMoves :: Board -> Square -> Board
kingMoves player n =
  kingAttacks n .\ player

bishopMoves :: Board -> Board -> Square -> Board
bishopMoves allPieces player n =
  bishopAttacks allPieces n .\ player

rookMoves :: Board -> Board -> Square -> Board
rookMoves allPieces player n =
  rookAttacks allPieces n .\ player

queenMoves :: Board -> Board -> Square -> Board
queenMoves allPieces player n =
  queenAttacks allPieces n .\ player


pawnAdvances :: Board -> Color -> Board -> Board
pawnAdvances allPieces color board = case color of
  White -> board << 8 .| ((rank_2 & board << 8) .\ allPieces) << 8
  Black -> board >> 8 .| ((rank_7 & board >> 8) .\ allPieces) << 8

pawnAttacks :: Color -> Board -> Board
pawnAttacks color board = case color of
  White -> (board .\ file_A) << 7 .| (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9 .| (board .\ file_H) >> 7

knightAttacks :: Square -> Board
knightAttacks n = knightMovesVec !! n

kingAttacks :: Square -> Board
kingAttacks n = kingMovesVec !! n

bishopAttacks :: Board -> Square -> Board
bishopAttacks allPieces n =
     sliding lsb (northEastMovesVec !!) allPieces n
  .| sliding lsb (northWestMovesVec !!) allPieces n
  .| sliding msb (southWestMovesVec !!) allPieces n
  .| sliding msb (southEastMovesVec !!) allPieces n

rookAttacks :: Board -> Square -> Board
rookAttacks allPieces n =
     sliding lsb (northMovesVec !!) allPieces n
  .| sliding lsb (eastMovesVec !!) allPieces n
  .| sliding msb (westMovesVec !!) allPieces n
  .| sliding msb (southMovesVec !!) allPieces n

queenAttacks :: Board -> Square -> Board
queenAttacks allPieces n =
     rookAttacks allPieces n
  .| bishopAttacks allPieces n


sliding ::  (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
sliding firstBlocker lookupMove allPieces n
  | blockers == 0 = ray
  | otherwise    = ray ^ lookupMove (firstBlocker (ray & allPieces))
  where
    ray = lookupMove n
    blockers = ray & allPieces
