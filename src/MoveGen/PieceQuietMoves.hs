module MoveGen.PieceQuietMoves (allQuietMoves) where


import           AppPrelude

import           Constants.Boards
import           Data.Bits
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks
import           MoveGen.PieceCaptures


-- Quiet moves legal move generator:
-- - Non capture moves except for pawn promotions

allQuietMoves :: Position -> [Move]
allQuietMoves pos@Position {..}

  | allCheckers == 0                            = genMoves id
  | leapingCheckers /= 0 || ones allCheckers > 1 = allKingMoves
  | otherwise                                  = genMoves blockChecker

  where
    genMoves = quietMovesHelper allPieces king allKingMoves pos
    blockChecker board = board & checkerRay
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingMoves =
      foldBoardSquares King
      (kingMoves allPieces attacked castling (player&rooks) king) [] kingSquare
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

    $ foldBoardMoves   Knight (f . knightMoves noPieces)
                              (unpinned&knights)

    $ foldBoardMoves   Bishop (f . bishopMoves noPieces
                                     allPieces pinnedPieces king)
                              (player&bishops)

    $ foldBoardMoves   Rook   (f . rookCaptures noPieces
                                     allPieces pinnedPieces king)
                              (player&rooks)

    $ foldBoardMoves   Queen  (f . queenCaptures noPieces
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


knightMoves :: Board -> Square -> Board
knightMoves noPieces n =
  knightAttacks n & noPieces


kingMoves :: Board -> Board -> Board -> Board -> Board -> Square -> Board
kingMoves allPieces attacked castling rooks king n =
  (kingAttacks n
  .| kingCastlingMoves allPieces attacked castling rooks king n)
  .\ (attacked .| allPieces)


bishopMoves :: Board -> Board -> Board -> Board -> Square -> Board
bishopMoves noPieces allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingBishopRay king n
  | otherwise              = attacks
  where
    attacks = bishopAttacks allPieces n & noPieces


kingCastlingMoves :: Board -> Board -> Board -> Board -> Board -> Square -> Board
kingCastlingMoves allPieces attacked castling rooks king n =

  (shortCastlingCond
      * toEnum (ones (castling & rooks & file_H))
      * ((castling & king) << 2))
  .|
  (longCastlingCond
      * toEnum (ones (castling & rooks & file_A))
      * ((castling & king) >> 2))
  where
    shortCastlingCond = 1
     - min 1 (shortCastleSliding & collisions .| inCheck)
    longCastlingCond = 1
     - min 1 (longCastleSliding & collisions .| inCheck
              .| allPieces & kingRank & file_B)
    collisions = kingRank & (attacked .| allPieces)
    kingRank = fileMovesVec !! n
    inCheck = king & attacked
