module MoveGen.PieceMoves where

import           AppPrelude       hiding (Vector)

import           Constants.Boards
import           Data.Bits        (Bits (bit))
import           Data.Vector      as Vector
import           Models.Move
import           Models.Piece
import           Models.Position


{-# INLINE  isKingInCheck #-}
isKingInCheck :: Position -> Bool
isKingInCheck pos@Position {..} =
  player&kings & allEnemyAttacks pos /= 0

{-# INLINE  getDirectCheckers #-}
getDirectCheckers :: Position -> Board
getDirectCheckers Position {..} =
  player & checkers
  where
    checkers = pawnAttacks (reverseColor color) (enemy&kings)
              .| knightAttacks (lsb (enemy&kings))

{-# INLINE  getSliderCheckers #-}
getSliderCheckers :: Board -> Position -> Board
getSliderCheckers checkerRays Position {..} =
  player & checkerRays & (bishops .| rooks .| queens)

{-# INLINE  getEnemyKingSliderRays #-}
getEnemyKingSliderRays :: Position -> Board
getEnemyKingSliderRays Position {..} =
  queenAttacks player (lsb (enemy&kings))

{-# INLINE  getEnemyKingCheckerRays #-}
getEnemyKingCheckerRays :: Position -> Board
getEnemyKingCheckerRays Position {..} =
  queenAttacks (enemy .| player) (lsb (enemy&kings))

{-# INLINE  getPinnedPieces #-}
getPinnedPieces :: Board -> Board -> Position -> Board
getPinnedPieces checkerRays sliderRays Position {..} =
  checkerRays & enemy &
  (foldMapBoard (bishopAttacks allPieces) (attackers & bishops)
  .| foldMapBoard (rookAttacks allPieces) (attackers & rooks)
  .| foldMapBoard (queenAttacks allPieces) (attackers & queens))
  where
    attackers = sliderRays & player
    allPieces = player .| enemy

{-# INLINE  getKingQueenRay #-}
getKingQueenRay :: Board -> Int -> Board
getKingQueenRay king n
  | file & king /= 0              = file
  | rank & king /= 0              = rank
  | diag & king /= 0              = diag
  | otherwise                    = antiDiag
  where
    file = fileMovesVec !! n
    rank = rankMovesVec !! n
    diag = diagMovesVec !! n
    antiDiag = antiDiagMovesVec !! n

{-# INLINE  getKingBishopRay #-}
getKingBishopRay :: Board -> Int -> Board
getKingBishopRay king n
  | diag & king /= 0              = diag
  | otherwise                    = antiDiag
  where
    diag = diagMovesVec !! n
    antiDiag = antiDiagMovesVec !! n

{-# INLINE  getKingRookRay #-}
getKingRookRay :: Board -> Int -> Board
getKingRookRay king n
  | file & king /= 0              = file
  | otherwise                    = rank
  where
    file = fileMovesVec !! n
    rank = rankMovesVec !! n


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
  Position {pawns, knights, bishops, rooks, queens, kings} =
     pawnAttacks  color                     (player&pawns)
  .| foldMapBoard knightAttacks             (player&knights)
  .| foldMapBoard (bishopAttacks allPieces) (player&bishops)
  .| foldMapBoard (rookAttacks allPieces)   (player&rooks)
  .| foldMapBoard (queenAttacks allPieces)  (player&queens)
  .| kingAttacks                       (lsb (player&kings))
  where
    allPieces = player .| (enemy .\ kings)

{-# INLINE  allLegalMoves #-}
allLegalMoves :: Position -> Vector Move
allLegalMoves pos@Position {..}

  | allCheckers == 0    = genMoves id
  | allCheckers > 1    = Vector.fromList allKingMoves
  | sliderCheckers /= 0 = genMoves captureOrBlockChecker
  | otherwise          = genMoves captureChecker

  where
    genMoves f = allLegalMovesHelper f allKingMoves pos
    captureChecker board = board & allCheckers
    captureOrBlockChecker board = board & (allCheckers .| checkerRay)
    checkerRay = getKingQueenRay king (lsb allCheckers)
    allKingMoves = foldBoardSquares King
      (kingMoves allPieces player attacked castling (player&rooks) king)
      [] kingSquare
    allCheckers = directCheckers .| sliderCheckers
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings

{-# INLINE  allLegalMovesHelper #-}
allLegalMovesHelper :: (Board -> Board) -> [Move] -> Position -> Vector Move
allLegalMovesHelper f allKingMoves Position {..} =
  Vector.fromList
    $ foldBoardMoves   Pawn (f . pawnMoves allPieces player enemy enPassant color)                (unpinned&pawns)
    $ foldBoardMoves   Knight (f . knightMoves player)           (unpinned&knights)
    $ foldBoardMoves   Bishop (f . bishopMoves allPieces pinnedPieces king player) (player&bishops)
    $ foldBoardMoves   Rook   (f . rookMoves allPieces pinnedPieces king player)   (player&rooks)
    $ foldBoardMoves   Queen  (f . queenMoves allPieces pinnedPieces king player)  (player&queens)
      allKingMoves
    where
    allPieces = player .| enemy
    unpinned = player .\ pinnedPieces
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
bishopMoves :: Board -> Board -> Board -> Board -> Square -> Board
bishopMoves allPieces pinnedPieces king player n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingBishopRay king n
  where
    attacks = bishopAttacks allPieces n .\ player

{-# INLINE  rookMoves #-}
rookMoves :: Board -> Board -> Board -> Board -> Square -> Board
rookMoves allPieces pinnedPieces king player n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingRookRay king n
  where
    attacks = rookAttacks allPieces n .\ player

{-# INLINE  queenMoves #-}
queenMoves :: Board -> Board -> Board -> Board -> Square -> Board
queenMoves allPieces pinnedPieces king player n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingQueenRay king n
  where
    attacks = queenAttacks allPieces n .\ player

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
