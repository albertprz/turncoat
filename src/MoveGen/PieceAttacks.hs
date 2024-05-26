module MoveGen.PieceAttacks (getLeapingCheckers, getSliderCheckers, getEnemyKingSliderRays, getBishopCheckerRays, getRookCheckerRays, getKingBishopRay, getPinnedPieces, getEnPassantPinnedPawns, getKingQueenRay, getKingRookRay, allPlayerAttacks, knightAttacks, bishopAttacks, rookAttacks, queenAttacks, kingAttacks, pawnAttacks, pawnDiagAttacks, pawnAntiDiagAttacks ) where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Piece
import           Models.Position


{-# INLINE  getLeapingCheckers #-}
getLeapingCheckers :: Position -> Board
getLeapingCheckers Position {..} =
  player & checkers
  where
    checkers = pawns   & pawnAttacks   (reverseColor color) (enemy&kings)
            .| knights & knightAttacks (lsb (enemy&kings))


{-# INLINE  getSliderCheckers #-}
getSliderCheckers :: Board -> Board -> Board -> Position -> Board
getSliderCheckers bishopCheckerRays rookCheckerRays queenCheckerRays
   Position {..} =
  player & checkers
  where
    checkers = bishopCheckerRays & bishops
            .| rookCheckerRays   & rooks
            .| queenCheckerRays  & queens


{-# INLINE  getEnemyKingSliderRays #-}
getEnemyKingSliderRays :: Position -> Board
getEnemyKingSliderRays Position {..} =
  queenAttacks player (lsb (enemy&kings))


{-# INLINE  getBishopCheckerRays #-}
getBishopCheckerRays :: Position -> Board
getBishopCheckerRays Position {..} =
  bishopAttacks (enemy .| player) (lsb (enemy&kings))


{-# INLINE  getRookCheckerRays #-}
getRookCheckerRays :: Position -> Board
getRookCheckerRays Position {..} =
  rookAttacks (enemy .| player) (lsb (enemy&kings))


getPinnedPieces :: Board -> Board -> Board -> Position -> Board
getPinnedPieces bishopCheckerRays rookCheckerRays sliderRays Position {..} =
  enemy &
  (foldBoard bishopPins (attackers & bishops)
  .| foldBoard rookPins (attackers & rooks)
  .| foldBoard queenPins (attackers & queens))
  where
    bishopPins n = getKingBishopRay king n
      & bishopCheckerRays
      & bishopAttacks allPieces n
    rookPins n = getKingRookRay king n
      & rookCheckerRays
      & rookAttacks allPieces n
    queenPins n = getKingQueenRay king n
      & (bishopCheckerRays .| rookCheckerRays)
      & queenAttacks allPieces n
    attackers = sliderRays & player
    king = enemy & kings
    allPieces = player .| enemy


getEnPassantPinnedPawns :: Position -> Board
getEnPassantPinnedPawns pos@Position {..} =
  pawns & getPinnedPieces 0 rookCheckerRays sliderRays pos'
  where
    rookCheckerRays = getRookCheckerRays pos'
    sliderRays = getEnemyKingSliderRays pos' & enPassantRank
    pos' = pos {
      pawns = pawns ^ enPassantPawn,
      player = player ^ enPassantPawn
    }
    enPassantPawn = case color of
      White -> enPassant << 8
      Black -> enPassant >> 8
    enPassantSquare = lsb enPassantPawn
    enPassantRank = fileMovesVec !! enPassantSquare


{-# INLINE  getKingQueenRay #-}
getKingQueenRay :: Board -> Square -> Board
getKingQueenRay king n
  | file & king /= 0     = file
  | rank & king /= 0     = rank
  | diag & king /= 0     = diag
  | antiDiag & king /= 0 = antiDiag
  | otherwise           = 0
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n


{-# INLINE  getKingBishopRay #-}
getKingBishopRay :: Board -> Square -> Board
getKingBishopRay king n
  | diag & king /= 0     = diag
  | antiDiag & king /= 0 = antiDiag
  | otherwise           = 0
  where
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n


{-# INLINE  getKingRookRay #-}
getKingRookRay :: Board -> Square -> Board
getKingRookRay king n
  | file & king /= 0 = file
  | rank & king /= 0 = rank
  | otherwise       = 0
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n


{-# INLINE  allPlayerAttacks #-}
allPlayerAttacks :: Position -> Board
allPlayerAttacks pos@Position {..} =
  allAttacks player enemy color pos


{-# INLINE  allAttacks #-}
allAttacks :: Board -> Board -> Color -> Position -> Board
allAttacks player enemy color
  Position {pawns, knights, bishops, rooks, queens, kings} =
     pawnAttacks  color                  (player&pawns)
  .| foldBoard knightAttacks             (player&knights)
  .| foldBoard (bishopAttacks allPieces) (player&bishops)
  .| foldBoard (rookAttacks allPieces)   (player&rooks)
  .| foldBoard (queenAttacks allPieces)  (player&queens)
  .| kingAttacks                    (lsb (player&kings))
  where
    allPieces = player .| (enemy .\ kings)


{-# INLINE  pawnAttacks #-}
pawnAttacks :: Color -> Board -> Board
pawnAttacks color board = case color of
  White -> (board .\ file_A) << 7 .| (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9 .| (board .\ file_H) >> 7


{-# INLINE  pawnDiagAttacks #-}
pawnDiagAttacks :: Color -> Board -> Board
pawnDiagAttacks color board = case color of
  White -> (board .\ file_H) << 9
  Black -> (board .\ file_A) >> 9


{-# INLINE  pawnAntiDiagAttacks #-}
pawnAntiDiagAttacks :: Color -> Board -> Board
pawnAntiDiagAttacks color board = case color of
  White -> (board .\ file_A) << 7
  Black -> (board .\ file_H) >> 7


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
    blockerRay = lookupMove firstBlocker
    firstBlocker = findBlocker blockers
    blockers = ray & allPieces
    ray = lookupMove n
