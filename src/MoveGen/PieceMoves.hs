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

{-# INLINE  getLeapingCheckers #-}
getLeapingCheckers :: Position -> Board
getLeapingCheckers Position {..} =
  player & checkers
  where
    checkers = (pawns   & pawnAttacks   (reverseColor color) (enemy&kings))
            .| (knights & knightAttacks (lsb (enemy&kings)))

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

{-# INLINE  getPinnedPieces #-}
getPinnedPieces :: Board -> Board -> Board -> Position -> Board
getPinnedPieces bishopCheckerRays rookCheckerRays sliderRays Position {..} =
  enemy &
  (foldMapBoard bishopPins (attackers & bishops)
  .| foldMapBoard rookPins (attackers & rooks)
  .| foldMapBoard queenPins (attackers & queens))
  where
    attackers = sliderRays & player
    bishopPins = (bishopCheckerRays &) . bishopAttacks allPieces
    rookPins = (rookCheckerRays &) . rookAttacks allPieces
    queenPins n = bishopPins n .| rookPins n
    allPieces = player .| enemy

{-# INLINE  getKingQueenRay #-}
getKingQueenRay :: Board -> Square -> Board
getKingQueenRay king n
  | file & king /= 0              = file
  | rank & king /= 0              = rank
  | diag & king /= 0              = diag
  | otherwise                    = antiDiag
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n

{-# INLINE  getKingBishopRay #-}
getKingBishopRay :: Board -> Square -> Board
getKingBishopRay king n
  | diag & king /= 0              = diag
  | otherwise                    = antiDiag
  where
    diag = antiDiagMovesVec !! n
    antiDiag = diagMovesVec !! n

{-# INLINE  getKingRookRay #-}
getKingRookRay :: Board -> Square -> Board
getKingRookRay king n
  | file & king /= 0              = file
  | otherwise                    = rank
  where
    file = rankMovesVec !! n
    rank = fileMovesVec !! n


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

  | allCheckers == 0      = genMoves id
  | ones allCheckers > 1 = Vector.fromList allKingMoves
  | sliderCheckers /= 0   = genMoves captureOrBlockChecker
  | otherwise            = genMoves captureChecker

  where
    genMoves = allLegalMovesHelper allPieces king allKingMoves pos
    captureChecker board = board & allCheckers
    captureOrBlockChecker board = board & (allCheckers .| checkerRay)
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingMoves = foldBoardSquares King
      (kingMoves allPieces player attacked castling (player&rooks) king)
      [] kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings

{-# INLINE  allLegalMovesHelper #-}
allLegalMovesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board) -> Vector Move
allLegalMovesHelper allPieces king allKingMoves Position {..} f =
  Vector.fromList
    $ foldBoardMoves   Pawn (f . pawnMoves allPieces player enemy enPassant color)                (unpinned&pawns)
    $ foldBoardMoves   Pawn (f . filePinnedPawnMoves allPieces enemy color)                filePinnedPawns
    $ foldBoardMoves   Pawn (f . diagPinnedPawnMoves enemy color)                diagPinnedPawns
    $ foldBoardMoves   Pawn (f . antiDiagPinnedPawnMoves enemy color)                antiDiagPinnedPawns
    $ foldBoardMoves   Knight (f . knightMoves player)           (unpinned&knights)
    $ foldBoardMoves   Bishop (f . bishopMoves allPieces pinnedPieces king player) (player&bishops)
    $ foldBoardMoves   Rook   (f . rookMoves allPieces pinnedPieces king player)   (player&rooks)
    $ foldBoardMoves   Queen  (f . queenMoves allPieces pinnedPieces king player)  (player&queens)
      allKingMoves
    where
    unpinned = player .\ pinnedPieces
    pinnedPawns = pinnedPieces & pawns
    filePinnedPawns = pinnedPawns & kingFile
    diagPinnedPawns = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingFile = rankMovesVec !! kingSquare
    kingDiag = antiDiagMovesVec !! kingSquare
    kingAntiDiag = diagMovesVec !! kingSquare
    kingSquare = lsb king


{-# INLINE  pawnMoves #-}
pawnMoves :: Board -> Board -> Board -> Board -> Color -> Square -> Board
pawnMoves allPieces player enemy enPassant color n =
  ((pawnAdvances allPieces color board .\ enemy)
  .| pawnAttacks color board & (enemy .| enPassant))
  .\ player
  where
    board = toBoard n

{-# INLINE  filePinnedPawnMoves #-}
filePinnedPawnMoves :: Board -> Board -> Color -> Square -> Board
filePinnedPawnMoves allPieces enemy color n =
  pawnAdvances allPieces color board .\ enemy
  where
    board = toBoard n

{-# INLINE  diagPinnedPawnMoves #-}
diagPinnedPawnMoves :: Board -> Color -> Square -> Board
diagPinnedPawnMoves enemy color n =
  pawnDiagAttacks color board & enemy
  where
    board = toBoard n

{-# INLINE  antiDiagPinnedPawnMoves #-}
antiDiagPinnedPawnMoves :: Board -> Color -> Square -> Board
antiDiagPinnedPawnMoves enemy color n =
  pawnAntiDiagAttacks color board & enemy
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
      = bit (ones (castling & rooks & file_A & kingFile))
        * ((castling & king) << 2)

  | longCastleSliding & collisions .| inCheck
    .| allPieces & kingFile & file_B == 0
      = bit (ones (castling & rooks & file_H & kingFile))
        * ((castling & king) >> 2)

  | otherwise = 0
  where
    collisions = kingFile & (attacked .| allPieces)
    kingFile = fileMovesVec !! n
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
    ray = lookupMove n
    blockerRay = lookupMove firstBlocker
    firstBlocker = findBlocker blockers
    blockers = ray & allPieces
