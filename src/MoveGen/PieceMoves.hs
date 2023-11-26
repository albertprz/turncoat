module MoveGen.PieceMoves where

import           AppPrelude

import           Data.Bits.Extras    (Ranked (lsb), msb)
import           Models.Board
import           Models.Piece
import           MoveGen.PieceBoards


pawnAdvances :: Board -> Color -> Square -> Board
pawnAdvances allPieces color n = case color of
  White -> pos << 8 .| ((rank_2 & pos << 8) .\ allPieces) << 8
  Black -> pos >> 8 .| ((rank_7 & pos >> 8) .\ allPieces) << 8
  where
    pos = position n

pawnAttacks :: Color -> Square -> Board
pawnAttacks color n = case color of
  White -> (pos .\ file_A) << 9 .| (pos .\ file_H) << 7
  Black -> (pos .\ file_A) >> 7 .| (pos .\ file_H) >> 9
  where
    pos = position n

knightAttacks :: Square -> Board
knightAttacks n = knightMoves !! n

bishopAttacks :: Board -> Square -> Board
bishopAttacks allPieces n =
  sliding lsb (northEastMoves !!) allPieces n
  .| sliding lsb (northWestMoves !!) allPieces n
  .| sliding msb (southWestMoves !!) allPieces n
  .| sliding msb (southEastMoves !!) allPieces n

rookAttacks :: Board -> Square -> Board
rookAttacks allPieces n =
  sliding lsb (westMoves !!) allPieces n
  .| sliding lsb (northMoves !!) allPieces n
  .| sliding msb (eastMoves !!) allPieces n
  .| sliding msb (southMoves !!) allPieces n

queenAttacks :: Board -> Square -> Board
queenAttacks allPieces n =
  rookAttacks allPieces n
  .| bishopAttacks allPieces n

kingAttacks :: Square -> Board
kingAttacks n = kingMoves !! n


sliding ::  (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
sliding firstBlocker lookupMove allPieces n =
  ray ^ blockerRay
  where
    ray = lookupMove n
    blockerRay = lookupMove (firstBlocker (ray & allPieces))
