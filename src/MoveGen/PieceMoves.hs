module MoveGen.PieceMoves where

import           AppPrelude       hiding (Vector)

import           Constants.Boards
import           Data.Bits        (Bits (bit))
import           Data.Vector      as Vector
import           Models.Move
import           Models.Piece
import           Models.Position


{-# INLINE  kingInCheck #-}
kingInCheck :: Position -> Bool
kingInCheck pos@Position {..} =
  player&kings & allEnemyAttacks pos /= 0

{-# INLINE  allPlayerAttacks #-}
allPlayerAttacks :: Position -> Board
allPlayerAttacks pos@Position {..} =
  allAttacks player enemy color pos

{-# INLINE  allEnemyAttacks #-}
allEnemyAttacks :: Position -> Board
allEnemyAttacks pos@Position {..} =
  allAttacks enemy player (reverseColor color) pos

{-# INLINE  allAttacks #-}
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
    allPieces = player .| (enemy .\ kings)

{-# INLINE  allMoves #-}
allMoves :: Position -> Vector Move
allMoves (Position {..}) = Vector.fromList
  $ foldBoardMoves   Pawn (pawnMoves allPieces player enemy enPassant color)
                                                           (player&pawns)
  $ foldBoardMoves   Knight (knightMoves player)           (player&knights)
  $ foldBoardMoves   Bishop (bishopMoves allPieces player) (player&bishops)
  $ foldBoardMoves   Rook   (rookMoves allPieces player)   (player&rooks)
  $ foldBoardMoves   Queen  (queenMoves allPieces player)  (player&queens)
  $ foldBoardSquares King
    (kingMoves allPieces player attacked castling (player&rooks) king)
    [] kingSquare
  where
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings

{-# INLINE  pawnMoves #-}
pawnMoves :: Board -> Board -> Board -> Board -> Color -> Square -> Board
pawnMoves allPieces player enemy enPassant color n =
  ((pawnAdvances allPieces color board .\ enemy)
  .| pawnAttacks color board & (enemy .| enPassant))
  .\ player
  where
    board = toBoard n

{-# INLINE  pawnAdvances #-}
pawnAdvances :: Board -> Color -> Board -> Board
pawnAdvances allPieces color board = case color of
  White -> board << 8 .| ((rank_2 & board) << 8 .\ allPieces) << 8
  Black -> board >> 8 .| ((rank_7 & board) >> 8 .\ allPieces) >> 8

{-# INLINE  knightMoves #-}
knightMoves :: Board -> Square -> Board
knightMoves player n =
  knightAttacks n .\ player

{-# INLINE  kingMoves #-}
kingMoves :: Board -> Board -> Board -> Board -> Board -> Board -> Square -> Board
kingMoves allPieces player attacked castling rooks king n =
  (kingAttacks n
  .| kingCastlingMoves allPieces attacked castling rooks king n)
  .\ (attacked .| player)

{-# INLINE  kingCastlingMoves #-}
kingCastlingMoves :: Board -> Board -> Board -> Board -> Board -> Int -> Board
kingCastlingMoves allPieces attacked castling rooks king n

  | shortCastleSliding & collisions .| inCheck == 0
      = bit (ones (castling & rooks & file_A & kingRank))
        * ((castling & king) << 2)

  | longCastleSliding & collisions .| inCheck
    .| allPieces & kingRank & file_B == 0
      = bit (ones (castling & rooks & file_H & kingRank))
        * ((castling & king) >> 2)

  | otherwise = 0
  where
    collisions = kingRank & (attacked .| allPieces)
    kingRank = fileMovesVec !! n
    inCheck = king & attacked


{-# INLINE  bishopMoves #-}
bishopMoves :: Board -> Board -> Square -> Board
bishopMoves allPieces player n =
  bishopAttacks allPieces n .\ player

{-# INLINE  rookMoves #-}
rookMoves :: Board -> Board -> Square -> Board
rookMoves allPieces player n =
  rookAttacks allPieces n .\ player

{-# INLINE  queenMoves #-}
queenMoves :: Board -> Board -> Square -> Board
queenMoves allPieces player n =
  queenAttacks allPieces n .\ player

{-# INLINE  pawnAttacks #-}
pawnAttacks :: Color -> Board -> Board
pawnAttacks color board = case color of
  White -> (board .\ file_A) << 7 .| (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9 .| (board .\ file_H) >> 7

{-# INLINE  knightAttacks #-}
knightAttacks :: Square -> Board
knightAttacks n = knightMovesVec !! n

{-# INLINE  kingAttacks #-}
kingAttacks :: Square -> Board
kingAttacks n = kingMovesVec !! n

{-# INLINE  bishopAttacks #-}
bishopAttacks :: Board -> Square -> Board
bishopAttacks allPieces n =
     sliding lsb (northEastMovesVec !!) allPieces n
  .| sliding lsb (northWestMovesVec !!) allPieces n
  .| sliding msb (southWestMovesVec !!) allPieces n
  .| sliding msb (southEastMovesVec !!) allPieces n

{-# INLINE  rookAttacks #-}
rookAttacks :: Board -> Square -> Board
rookAttacks allPieces n =
     sliding lsb (northMovesVec !!) allPieces n
  .| sliding lsb (eastMovesVec !!) allPieces n
  .| sliding msb (westMovesVec !!) allPieces n
  .| sliding msb (southMovesVec !!) allPieces n

{-# INLINE  queenAttacks #-}
queenAttacks :: Board -> Square -> Board
queenAttacks allPieces n =
     rookAttacks allPieces n
  .| bishopAttacks allPieces n

{-# INLINE  sliding #-}
sliding :: (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
sliding findBlocker lookupMove allPieces n =
   ray ^ blockerRay
  where
    ray = lookupMove n
    blockerRay = lookupMove firstBlocker
    firstBlocker = findBlocker blockers
    blockers = ray & allPieces
