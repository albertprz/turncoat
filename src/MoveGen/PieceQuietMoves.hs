module MoveGen.PieceQuietMoves (allQuietMoves) where


import           AppPrelude

import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks
import           MoveGen.PieceCaptures
import           Utils.Board


-- Quiet moves legal move generator:
-- - Non capture moves except for pawn promotions

allQuietMoves :: Position -> [Move]
allQuietMoves pos@Position {..}

  | allCheckers == 0                                = genMoves id
  | leapingCheckers /= 0 || popCount allCheckers > 1 = allKingMoves
  | otherwise                                      = genMoves blockChecker

  where
    genMoves = quietMovesHelper allPieces king allKingMoves pos
    blockChecker board = board & checkerRay
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingMoves =
      foldBoardSquares King
      (kingQuietMoves allPieces attacked castling (player&rooks) king) [] kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings


quietMovesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board)
                 -> [Move]
quietMovesHelper allPieces king allKingMoves Position {..} !f =

    foldBoardMoves     Pawn   (f . pawnAdvances noPieces color
                                 . toBoard)
                              (unpinned&pawns .| filePinnedPawns)

    $ foldBoardMoves   Knight (f . knightCaptures noPieces)
                              (unpinned&knights)

    $ foldBoardMoves   Bishop (f . bishopQuietMoves noPieces
                                     allPieces pinnedPieces king)
                              (player&bishops)

    $ foldBoardMoves   Rook   (f . rookQuietMoves noPieces
                                     allPieces pinnedPieces king)
                              (player&rooks)

    $ foldBoardMoves   Queen  (f . queenQuietMoves noPieces
                                     allPieces pinnedPieces king)
                              (player&queens)

      allKingMoves

    where
    unpinned = player .\ pinnedPieces
    pinnedPawns = pinnedPieces & pawns
    filePinnedPawns = pinnedPawns & kingFile
    kingFile = rankMovesVec !! kingSquare
    kingSquare = lsb king
    noPieces = (~) allPieces


pawnAdvances :: Board -> Color -> Board -> Board
pawnAdvances noPieces color board = advances & noPieces
  where

    advances = case color of
      White -> (board .\ rank_7) << 8
           .| ((board & rank_2) << 8 & noPieces) << 8
      Black -> (board .\ rank_2) >> 8
           .| ((board & rank_7) >> 8 & noPieces) >> 8

bishopQuietMoves :: Board -> Board -> Board -> Board -> Square -> Board
bishopQuietMoves noPieces allPieces pinnedPieces king n =
  bishopMoves allPieces pinnedPieces king n & noPieces


rookQuietMoves :: Board -> Board -> Board -> Board -> Square -> Board
rookQuietMoves noPieces allPieces pinnedPieces king n =
  rookMoves allPieces pinnedPieces king n & noPieces


queenQuietMoves :: Board -> Board -> Board -> Board -> Square -> Board
queenQuietMoves noPieces allPieces pinnedPieces king n =
  queenMoves allPieces pinnedPieces king n & noPieces


kingQuietMoves :: Board -> Board -> Board -> Board -> Board -> Square -> Board
kingQuietMoves allPieces attacked castling rooks king n =
  (kingAttacks n
  .| kingCastlingMoves allPieces attacked castling rooks king n)
  .\ (attacked .| allPieces)


kingCastlingMoves :: Board -> Board -> Board -> Board -> Board -> Square -> Board
kingCastlingMoves allPieces attacked castling rooks king n =
  (shortCastlingCond
      * fromIntegral (popCount (castling & rooks & file_H))
      * ((castling & king) << 2))
  .|
  (longCastlingCond
      * fromIntegral (popCount (castling & rooks & file_A))
      * ((castling & king) >> 2))
  where
    shortCastlingCond =
     toReverseCondition (shortCastleSlidingFiles & collisions .| inCheck)
    longCastlingCond =
     toReverseCondition (longCastleSlidingFiles  & collisions
            .| inCheck
            .| allPieces & kingRank & file_B)
    collisions = kingRank & (attacked .| allPieces)
    kingRank = fileMovesVec !! n
    inCheck = king & attacked
