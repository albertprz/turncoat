module MoveGen.PieceCaptures where

import           AppPrelude

import           Constants.Boards
import           Data.Bits
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceMoves



{-# INLINE  allLegalCaptures #-}
allLegalCaptures :: Position -> [Move]
allLegalCaptures pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | sliderCheckers /= 0   = genCaptures captureChecker pawnCaptureOrBlockChecker
  | otherwise            = genCaptures captureChecker pawnCaptureChecker

  where
    genCaptures = allLegalCapturesHelper allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board & (allCheckers .| enPassantChecker)
    pawnCaptureOrBlockChecker board = board & (allCheckers .| checkerRay .| enPassantChecker)
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingCaptures = foldBoardSquares King (kingCaptures enemy) []
                                       kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker =
      let checker = allCheckers & pawns
      in enPassant & (checker << 8 .| checker >> 8)
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings


{-# INLINE  allLegalCapturesHelper #-}
allLegalCapturesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
allLegalCapturesHelper allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves   Pawn (g . pawnCapturesAndPromotions enemy allPieces enPassant color)                (unpinned&pawns)
    $ foldBoardMoves   Pawn (g . pawnPromotions allPieces color . toBoard)                filePinnedPawns
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
    filePinnedPawns = pinnedPawns & kingFile
    diagPinnedPawns = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingFile = rankMovesVec !! kingSquare
    kingDiag = antiDiagMovesVec !! kingSquare
    kingAntiDiag = diagMovesVec !! kingSquare
    kingSquare = lsb king


{-# INLINE  staticExchangeCaptures #-}
staticExchangeCaptures :: Square -> Position -> [Move]
staticExchangeCaptures target pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | sliderCheckers /= 0   = genCaptures captureChecker pawnCaptureOrBlockChecker
  | otherwise            = genCaptures captureChecker pawnCaptureChecker

  where
    genCaptures = staticExchangeCapturesHelper target allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board & (allCheckers .| enPassantChecker)
    pawnCaptureOrBlockChecker board = board & (allCheckers .| checkerRay .| enPassantChecker)
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingCaptures = foldBoardSquares King (kingCaptures enemy) []
                                       kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker =
      let checker = allCheckers & pawns
      in enPassant & (checker << 8 .| checker >> 8)
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings


{-# INLINE  staticExchangeCapturesHelper #-}
staticExchangeCapturesHelper :: Square -> Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
staticExchangeCapturesHelper target allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves   Pawn (g . pawnCapturesAndPromotions targetBoard allPieces enPassant color)                (unpinned&pawnAttackers)
    $ foldBoardMoves   Pawn (g . diagPinnedPawnMoves targetBoard color)                diagPinnedPawns
    $ foldBoardMoves   Pawn (g . antiDiagPinnedPawnMoves targetBoard color)                antiDiagPinnedPawns
    $ foldBoardMoves   Knight (f . knightCaptures targetBoard)           (unpinned&knightAttackers)
    $ foldBoardMoves   Bishop (f . bishopCaptures targetBoard allPieces pinnedPieces king) (player&bishopAttackers)
    $ foldBoardMoves   Rook   (f . rookCaptures targetBoard allPieces pinnedPieces king)   (player&rookAttackers)
    $ foldBoardMoves   Queen  (f . queenCaptures targetBoard allPieces pinnedPieces king)  (player&queenAttackers)
    allKingCaptures

    where
    pawnAttackers = pawns & pawnAttacks color targetBoard
    knightAttackers = knights & knightAttacks target
    bishopAttackers = bishops & bishopAttacks allPieces target
    rookAttackers = rooks & rookAttacks allPieces target
    queenAttackers = queens & bishopAttackers .| rookAttackers

    targetBoard = toBoard target
    unpinned = player .\ pinnedPieces
    pinnedPawns = pinnedPieces & pawns
    diagPinnedPawns = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingDiag = antiDiagMovesVec !! kingSquare
    kingAntiDiag = diagMovesVec !! kingSquare
    kingSquare = lsb king

{-# INLINE  pawnCapturesAndPromotions #-}
pawnCapturesAndPromotions :: Board -> Board -> Board -> Color -> Square -> Board
pawnCapturesAndPromotions enemy allPieces enPassant color n =
  pawnPromotions allPieces color board
  .| pawnCaptures enemy enPassant color board
  where
    board = toBoard n

{-# INLINE  pawnPromotions #-}
pawnPromotions :: Board -> Color -> Board -> Board
pawnPromotions allPieces color board =
  promotions .\ allPieces
  where
    promotions = case color of
      White -> (rank_7 & board) << 8
      Black -> (rank_2 & board) >> 8

{-# INLINE  pawnCaptures #-}
pawnCaptures :: Board -> Board -> Color -> Board -> Board
pawnCaptures enemy enPassant color board =
  pawnAttacks color board & (enemy .| enPassant)

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
  | testBit pinnedPieces n = attacks & getKingBishopRay king n
  | otherwise              = attacks
  where
    attacks = bishopCaptureAttacks allPieces n & enemy

{-# INLINE  rookCaptures #-}
rookCaptures :: Board -> Board -> Board -> Board -> Square -> Board
rookCaptures enemy allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingRookRay king n
  | otherwise              = attacks
  where
    attacks = rookCaptureAttacks allPieces n & enemy

{-# INLINE  queenCaptures #-}
queenCaptures :: Board -> Board -> Board -> Board -> Square -> Board
queenCaptures enemy allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingQueenRay king n
  | otherwise              = attacks
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
