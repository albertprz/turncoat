module MoveGen.PieceMoves where

import           AppPrelude

import           Data.List           ((!!))
import           Models.Board
import           Models.Piece
import           MoveGen.PieceBoards


pawnAdvances :: Color -> Int -> Board
pawnAdvances color n = case color of
  White -> pos << 8 .| rank_2 & pos << 16
  Black -> pos >> 8 .| rank_7 & pos >> 16
  where
    pos = position n

pawnAttacks :: Color -> Int -> Board
pawnAttacks color n = case color of
  White -> (pos .\ file_A) << 9 .| (pos .\ file_H) << 7
  Black -> (pos .\ file_A) >> 7 .| (pos .\ file_H) >> 9
  where
    pos = position n

knightAttacks :: Int -> Board
knightAttacks n = knightMoves !! n

bishopAttacks :: Board -> Int -> Board
bishopAttacks allPieces n =
  slidingAttacks allPieces
    (northEastMoves !! n) (northWestMoves !! n)
    (southWestMoves !! n) (southEastMoves !! n)
    (position n)

rookAttacks :: Board -> Int -> Board
rookAttacks allPieces n =
  slidingAttacks allPieces
    (westMoves !! n) (northMoves !! n)
    (eastMoves !! n) (southMoves !! n)
    (position n)

queenAttacks :: Board -> Int -> Board
queenAttacks allPieces n =
  rookAttacks allPieces n .| bishopAttacks allPieces n

kingAttacks :: Int -> Board
kingAttacks n = kingMoves !! n



-- Sliding
slidingAttacks :: Board -> Board -> Board -> Board -> Board -> Board -> Board
slidingAttacks allPieces ray1 ray2 ray3 ray4 pos =
  slidingAbove allPieces ray1 pos
  .| slidingAbove allPieces ray2 pos
  .| slidingBelow allPieces ray3 pos
  .| slidingBelow allPieces ray4 pos

slidingAbove :: Board -> Board -> Board -> Board
slidingAbove allPieces ray pos =
  if collisions > pos
    then ((collisions - pos) << 1) & ray
    else ray
  where
    collisions = 1 <<| trailZeros (ray & allPieces)

slidingBelow :: Board -> Board -> Board -> Board
slidingBelow pos allPieces ray =
  if collisions < pos && collisions /= 0
    then (pos - collisions) & ray
    else ray
  where
    collisions = if collisionMask /= 0
      then 1 << (63 - leadZeros collisionMask)
      else 0
    collisionMask = ray & allPieces
