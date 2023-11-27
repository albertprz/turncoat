module MoveGen.PieceMoves where

import           AppPrelude

import           Data.Bits.Extras    (Ranked (lsb), msb)
import           Models.Board
import           Models.Piece
import           Models.Position
import           MoveGen.PieceBoards


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
  pawnAttacks color (player&pawns)
  .| foldMapBoard (.|) knightAttacks (player&knights)
  .| foldMapBoard (.|) (bishopAttacks allPieces) (player&bishops)
  .| foldMapBoard (.|) (rookAttacks allPieces) (player&rooks)
  .| foldMapBoard (.|) (queenAttacks allPieces) (player&queens)
  .| foldMapBoard (.|) kingAttacks (player&kings)
  where
    allPieces = player .| enemy


pawnAdvances :: Board -> Color -> Board -> Board
pawnAdvances allPieces color board = case color of
  White -> board << 8 .| ((rank_2 & board << 8) .\ allPieces) << 8
  Black -> board >> 8 .| ((rank_7 & board >> 8) .\ allPieces) << 8

pawnAttacks :: Color -> Board -> Board
pawnAttacks color board = case color of
  White -> (board .\ file_A) << 7 .| (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9 .| (board .\ file_H) >> 7

knightAttacks :: Square -> Board
knightAttacks n = knightMoves !! n

kingAttacks :: Square -> Board
kingAttacks n = kingMoves !! n

bishopAttacks :: Board -> Square -> Board
bishopAttacks allPieces n =
  sliding lsb (northEastMoves !!) allPieces n
  .| sliding lsb (northWestMoves !!) allPieces n
  .| sliding msb (southWestMoves !!) allPieces n
  .| sliding msb (southEastMoves !!) allPieces n

rookAttacks :: Board -> Square -> Board
rookAttacks allPieces n =
   sliding lsb (northMoves !!) allPieces n
  .| sliding lsb (eastMoves !!) allPieces n
  .| sliding msb (westMoves !!) allPieces n
  .| sliding msb (southMoves !!) allPieces n

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
