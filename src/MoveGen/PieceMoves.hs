module MoveGen.PieceMoves where

import           AppPrelude

import           Constants.Boards
import           Data.Bits        (Bits (bit))
import           Models.Move
import           Models.Piece
import           Models.Position


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

allMoves :: Position -> [Move]
allMoves (Position {..}) =
    foldBoardMoves   Pawn (pawnMoves allPieces player enemy enPassant color)
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

pawnMoves :: Board -> Board -> Board -> Board -> Color -> Square -> Board
pawnMoves allPieces player enemy enPassant color n =
  ((pawnAdvances allPieces color board .\ enemy)
  .| pawnAttacks color board & (enemy .| enPassant))
  .\ player
  where
    board = toBoard n

pawnAdvances :: Board -> Color -> Board -> Board
pawnAdvances allPieces color board = case color of
  White -> board << 8 .| ((rank_2 & board) << 8 .\ allPieces) << 8
  Black -> board >> 8 .| ((rank_7 & board) >> 8 .\ allPieces) >> 8

knightMoves :: Board -> Square -> Board
knightMoves player n =
  knightAttacks n .\ player

kingMoves :: Board -> Board -> Board -> Board -> Board -> Board -> Square -> Board
kingMoves allPieces player attacked castling rooks king n =
  (kingAttacks n
  .| kingCastlingMoves allPieces attacked castling rooks king n)
  .\ (attacked .| player)

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


bishopMoves :: Board -> Board -> Square -> Board
bishopMoves allPieces player n =
  bishopAttacks allPieces n .\ player

rookMoves :: Board -> Board -> Square -> Board
rookMoves allPieces player n =
  rookAttacks allPieces n .\ player

queenMoves :: Board -> Board -> Square -> Board
queenMoves allPieces player n =
  queenAttacks allPieces n .\ player

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

sliding :: (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
sliding findBlocker lookupMove allPieces n =
   ray ^ blockerRay
  where
    ray = lookupMove n
    blockerRay = lookupMove firstBlocker
    firstBlocker = findBlocker blockers
    blockers = ray & allPieces
