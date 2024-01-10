module MoveGen.PieceCaptures where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceMoves



{-# INLINE  allLegalCaptures #-}
allLegalCaptures :: Board -> Position -> [Move]
allLegalCaptures target pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | otherwise            = genCaptures captureChecker pawnCaptureChecker

  where
    genCaptures = allLegalCapturesHelper target allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board & (allCheckers .| enPassantChecker)
    allKingCaptures = foldBoardSquares King (kingCaptures target) []
                                       kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker = toEnum
        (ones (allCheckers & pawns & (enPassant << 8 .| enPassant >> 8)))
        * enPassant
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings

{-# INLINE  allLegalCapturesHelper #-}
allLegalCapturesHelper :: Board -> Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
allLegalCapturesHelper target allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves   Pawn (g . pawnCaptures target enPassant color)                (unpinned&pawns)
    $ foldBoardMoves   Pawn (g . diagPinnedPawnMoves target color)                diagPinnedPawns
    $ foldBoardMoves   Pawn (g . antiDiagPinnedPawnMoves target color)                antiDiagPinnedPawns
    $ foldBoardMoves   Knight (f . knightCaptures target)           (unpinned&knights)
    $ foldBoardMoves   Bishop (f . bishopCaptures target allPieces pinnedPieces king) (player&bishops)
    $ foldBoardMoves   Rook   (f . rookCaptures target allPieces pinnedPieces king)   (player&rooks)
    $ foldBoardMoves   Queen  (f . queenCaptures target allPieces pinnedPieces king)  (player&queens)
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
pawnCaptures target enPassant color n =
  pawnAttacks color board & (target .| enPassant)
  where
    board = toBoard n

{-# INLINE  knightCaptures #-}
knightCaptures :: Board -> Square -> Board
knightCaptures target n =
  knightAttacks n & target

{-# INLINE  kingCaptures #-}
kingCaptures :: Board -> Square -> Board
kingCaptures target n =
  kingAttacks n & target

{-# INLINE  bishopCaptures #-}
bishopCaptures :: Board -> Board -> Board -> Board -> Square -> Board
bishopCaptures target allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingBishopRay king n
  where
    attacks = bishopCaptureAttacks allPieces n & target

{-# INLINE  rookCaptures #-}
rookCaptures :: Board -> Board -> Board -> Board -> Square -> Board
rookCaptures target allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingRookRay king n
  where
    attacks = rookCaptureAttacks allPieces n & target

{-# INLINE  queenCaptures #-}
queenCaptures :: Board -> Board -> Board -> Board -> Square -> Board
queenCaptures target allPieces pinnedPieces king n
  | pinnedPieces & toBoard n == 0 = attacks
  | otherwise                    = attacks & getKingQueenRay king n
  where
    attacks = queenCaptureAttacks allPieces n & target


{-# INLINE  bishopCaptureAttacks #-}
bishopCaptureAttacks :: Board -> Square -> Board
bishopCaptureAttacks target n =
     slidingCapture lsb (northEastMovesVec !!) target n
  .| slidingCapture lsb (northWestMovesVec !!) target n
  .| slidingCapture msb (southWestMovesVec !!) target n
  .| slidingCapture msb (southEastMovesVec !!) target n

{-# INLINE  rookCaptureAttacks #-}
rookCaptureAttacks :: Board -> Square -> Board
rookCaptureAttacks target n =
     slidingCapture lsb (northMovesVec !!) target n
  .| slidingCapture lsb (eastMovesVec !!) target n
  .| slidingCapture msb (westMovesVec !!) target n
  .| slidingCapture msb (southMovesVec !!) target n

{-# INLINE  queenCaptureAttacks #-}
queenCaptureAttacks :: Board -> Square -> Board
queenCaptureAttacks allPieces n =
     rookCaptureAttacks allPieces n
  .| bishopCaptureAttacks allPieces n

{-# INLINE  slidingCapture #-}
slidingCapture :: (Board -> Square) -> (Square -> Board) -> Board -> Square -> Board
slidingCapture findBlocker lookupMove target n =
   toBoard firstBlocker
  where
    firstBlocker = findBlocker blockers
    blockers = ray & target
    ray = lookupMove n
