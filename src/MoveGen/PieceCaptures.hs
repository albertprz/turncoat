module MoveGen.PieceCaptures where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceMoves


{-# INLINE  allLegalCaptures #-}
allLegalCaptures :: Position -> [Move]
allLegalCaptures pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | otherwise            = genCaptures captureChecker pawnCaptureChecker

  where
    genCaptures = allLegalCapturesHelper allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board & (allCheckers .| enPassantChecker)
    allKingCaptures = foldBoardSquares King (kingCaptures enemy) []
                                       kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker = toEnum
        (ones (allCheckers & pawns & (enPassant << 8 .| enPassant >> 8)))
        * enPassant
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings

{-# INLINE  allLegalCapturesHelper #-}
allLegalCapturesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
allLegalCapturesHelper allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves   Pawn (g . pawnCaptures enemy enPassant color)                (unpinned&pawns)
    $ foldBoardMoves   Pawn (g . diagPinnedPawnMoves enemy color)                diagPinnedPawns
    $ foldBoardMoves   Pawn (g . antiDiagPinnedPawnMoves enemy color)                antiDiagPinnedPawns
    $ foldBoardMoves   Knight (f . knightCaptures enemy)           (unpinned&knights)
    $ foldBoardMoves   Bishop (f . bishopCaptures enemy allPieces pinnedPieces king) (player&bishops)
    $ foldBoardMoves   Rook   (f . rookCaptures enemy allPieces pinnedPieces king)   (player&rooks)
    $ foldBoardMoves   Queen  (f . queenCaptures enemy allPieces pinnedPieces king)  (player&queens)
      allKingCaptures
    where
    unpinned = player .\ pinnedPieces
    pinnedPawns = pinnedPieces & pawns
    diagPinnedPawns = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingDiag = antiDiagMovesVec !! kingSquare
    kingAntiDiag = diagMovesVec !! kingSquare
    kingSquare = lsb king


{-# INLINE  pawnCaptures #-}
pawnCaptures :: Board -> Board -> Color -> Square -> Board
pawnCaptures enemy enPassant color n =
  pawnAttacks color board & (enemy .| enPassant)
  where
    board = toBoard n

{-# INLINE  knightCaptures #-}
knightCaptures :: Board -> Square -> Board
knightCaptures enemy n =
  knightAttacks n & enemy

{-# INLINE  kingCaptures #-}
kingCaptures :: Board -> Square -> Board
kingCaptures enemy n =
  kingAttacks n & enemy

{-# INLINE  bishopCaptures #-}
bishopCaptures :: Board -> Board -> Board -> Board -> Square -> Board
bishopCaptures enemy allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingBishopRay king n
  where
    attacks = bishopCaptureAttacks allPieces n & enemy

{-# INLINE  rookCaptures #-}
rookCaptures :: Board -> Board -> Board -> Board -> Square -> Board
rookCaptures enemy allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingRookRay king n
  where
    attacks = rookCaptureAttacks allPieces n & enemy

{-# INLINE  queenCaptures #-}
queenCaptures :: Board -> Board -> Board -> Board -> Square -> Board
queenCaptures enemy allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingQueenRay king n
  where
    attacks = queenCaptureAttacks allPieces n & enemy


{-# INLINE  bishopCaptureAttacks #-}
bishopCaptureAttacks :: Board -> Square -> Board
bishopCaptureAttacks enemy n =
     slidingCapture lsb (northEastMovesVec !!) enemy n
  .| slidingCapture lsb (northWestMovesVec !!) enemy n
  .| slidingCapture msb (southWestMovesVec !!) enemy n
  .| slidingCapture msb (southEastMovesVec !!) enemy n

{-# INLINE  rookCaptureAttacks #-}
rookCaptureAttacks :: Board -> Square -> Board
rookCaptureAttacks enemy n =
     slidingCapture lsb (northMovesVec !!) enemy n
  .| slidingCapture lsb (eastMovesVec !!) enemy n
  .| slidingCapture msb (westMovesVec !!) enemy n
  .| slidingCapture msb (southMovesVec !!) enemy n

{-# INLINE  queenCaptureAttacks #-}
queenCaptureAttacks :: Board -> Square -> Board
queenCaptureAttacks allPieces n =
     rookCaptureAttacks allPieces n
  .| bishopCaptureAttacks allPieces n

{-# INLINE  slidingCapture #-}
slidingCapture :: (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
slidingCapture findBlocker lookupMove enemy n =
   toBoard firstBlocker
  where
    firstBlocker = findBlocker blockers
    blockers = ray & enemy
    ray = lookupMove n
