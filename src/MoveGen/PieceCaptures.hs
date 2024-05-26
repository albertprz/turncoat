module MoveGen.PieceCaptures (allCaptures, staticExchangeCaptures, rookCaptures, queenCaptures) where

import           AppPrelude

import           Constants.Boards
import           Data.Bits
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks


-- Captures legal move generator:
-- - Piece captures
-- - En passant capture
-- - Pawn promotions

allCaptures :: Position -> [Move]
allCaptures pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | sliderCheckers /= 0   = genCaptures captureChecker
                                       pawnCaptureOrBlockChecker
  | otherwise            = genCaptures captureChecker
                                       pawnCaptureChecker

  where
    genCaptures =
      allCapturesHelper allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board
      & (allCheckers .| enPassantChecker)
    pawnCaptureOrBlockChecker board = board
      & (allCheckers .| enPassantChecker .| checkerRay)
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingCaptures =
      foldBoardSquares King (kingCaptures enemy attacked) [] kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker =
      let checker = allCheckers & pawns
      in enPassant & (checker << 8 .| checker >> 8)
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings


allCapturesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
allCapturesHelper allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves     Pawn   (g . pawnCapturesAndPromotions
                                     enemy noPieces enPassant color)
                              (unpinned&pawns)

    $ foldBoardMoves   Pawn   (g . pawnPromotions noPieces color
                                 . toBoard)
                              filePinnedPawns

    $ foldBoardMoves   Pawn   (g . diagPawnCaptures enemy color)
                              diagPinnedPawns

    $ foldBoardMoves   Pawn   (g . antiDiagPawnCaptures enemy color)
                              antiDiagPinnedPawns

    $ foldBoardMoves   Knight (f . knightCaptures enemy)
                              (unpinned&knights)

    $ foldBoardMoves   Bishop (f . bishopCaptures
                                     enemy allPieces pinnedPieces king)
                              (player&bishops)

    $ foldBoardMoves   Rook   (f . rookCaptures
                                     enemy allPieces pinnedPieces king)
                              (player&rooks)

    $ foldBoardMoves   Queen  (f . queenCaptures
                                     enemy allPieces pinnedPieces king)
                              (player&queens)
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
    noPieces = (~) allPieces


staticExchangeCaptures :: Square -> Position -> [Move]
staticExchangeCaptures target pos@Position {..}

  | allCheckers == 0      = genCaptures id id
  | ones allCheckers > 1 = allKingCaptures
  | sliderCheckers /= 0   = genCaptures captureChecker
                                       pawnCaptureOrBlockChecker
  | otherwise            = genCaptures captureChecker
                                       pawnCaptureChecker

  where
    genCaptures =
      staticExchangeCapturesHelper target allPieces king allKingCaptures pos
    captureChecker board = board & allCheckers
    pawnCaptureChecker board = board
      & (allCheckers .| enPassantChecker)
    pawnCaptureOrBlockChecker board = board
      & (allCheckers .| checkerRay .| enPassantChecker)
    checkerRay = getKingQueenRay king checkerSquare
      & queenAttacks allPieces kingSquare
      & queenAttacks allPieces checkerSquare
    checkerSquare = lsb allCheckers
    allKingCaptures =
      foldBoardSquares King (kingCaptures enemy attacked) [] kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker =
      let checker = allCheckers & pawns
      in enPassant & (checker << 8 .| checker >> 8)
    allPieces = player .| enemy
    kingSquare = lsb king
    king = player&kings


staticExchangeCapturesHelper :: Square -> Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
staticExchangeCapturesHelper target allPieces king allKingCaptures Position {..} f g =

    foldBoardMoves    Pawn   (g . pawnCaptures targetBoard enPassant color
                                . toBoard)
                             (unpinned&pawnAttackers)

    $ foldBoardMoves  Pawn   (g . diagPawnCaptures
                                    targetBoard color)
                             diagPinnedPawns

    $ foldBoardMoves  Pawn   (g . antiDiagPawnCaptures
                                    targetBoard color)
                             antiDiagPinnedPawns

    $ foldBoardMoves  Knight (f . knightCaptures targetBoard)
                             (unpinned&knightAttackers)

    $ foldBoardMoves  Bishop (f . bishopCaptures targetBoard
                                    allPieces pinnedPieces king)
                             (player&bishopAttackers)

    $ foldBoardMoves  Rook   (f . rookCaptures targetBoard
                                    allPieces pinnedPieces king)
                             (player&rookAttackers)

    $ foldBoardMoves  Queen  (f . queenCaptures targetBoard

                                    allPieces pinnedPieces king)
                             (player&queenAttackers)
    allKingCaptures

    where
    pawnAttackers = pawns & pawnAttacks (reverseColor color) targetBoard
    knightAttackers = knights & knightAttacks target
    bishopAttackers = bishops & bishopRays
    rookAttackers = rooks & rookRays
    queenAttackers = queens & (bishopRays .| rookRays)
    bishopRays = bishopAttacks allPieces target
    rookRays = rookAttacks allPieces target

    targetBoard = toBoard target
    unpinned = player .\ pinnedPieces
    pinnedPawns = pinnedPieces & pawns
    diagPinnedPawns = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingDiag = antiDiagMovesVec !! kingSquare
    kingAntiDiag = diagMovesVec !! kingSquare
    kingSquare = lsb king


pawnCapturesAndPromotions :: Board -> Board -> Board -> Color -> Square -> Board
pawnCapturesAndPromotions enemy noPieces enPassant color n =
  pawnPromotions noPieces color board
  .| pawnCaptures enemy enPassant color board
  where
    board = toBoard n

pawnPromotions :: Board -> Color -> Board -> Board
pawnPromotions noPieces color board =
  promotions & noPieces
  where
    promotions = case color of
      White -> (board & rank_7) << 8
      Black -> (board & rank_2) >> 8


pawnCaptures :: Board -> Board -> Color -> Board -> Board
pawnCaptures enemy enPassant color board =
  pawnAttacks color board & (enemy .| enPassant)

diagPawnCaptures :: Board -> Color -> Square -> Board
diagPawnCaptures enemy color n =
  pawnDiagAttacks color board & enemy
  where
    board = toBoard n

antiDiagPawnCaptures :: Board -> Color -> Square -> Board
antiDiagPawnCaptures enemy color n =
  pawnAntiDiagAttacks color board & enemy
  where
    board = toBoard n


knightCaptures :: Board -> Square -> Board
knightCaptures enemy n =
  knightAttacks n & enemy

kingCaptures :: Board -> Board -> Square -> Board
kingCaptures enemy attacked n =
  (kingAttacks n & enemy)
  .\ attacked

bishopCaptures :: Board -> Board -> Board -> Board -> Square -> Board
bishopCaptures enemy allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingBishopRay king n
  | otherwise              = attacks
  where
    attacks = bishopAttacks allPieces n & enemy

rookCaptures :: Board -> Board -> Board -> Board -> Square -> Board
rookCaptures enemy allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingRookRay king n
  | otherwise              = attacks
  where
    attacks = rookAttacks allPieces n & enemy

queenCaptures :: Board -> Board -> Board -> Board -> Square -> Board
queenCaptures enemy allPieces pinnedPieces king n
  | testBit pinnedPieces n = attacks & getKingQueenRay king n
  | otherwise              = attacks
  where
    attacks = queenAttacks allPieces n & enemy
