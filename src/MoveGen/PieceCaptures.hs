module MoveGen.PieceCaptures (allCaptures, staticExchangeCaptures, knightCaptures, bishopCaptures, rookCaptures, queenCaptures) where

import           AppPrelude

import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks
import           Utils.Board


-- Captures legal move generator:
-- - Piece captures
-- - En passant captures
-- - Pawn promotions

allCaptures :: Position -> [Move]
allCaptures pos@Position {..}

  | allCheckers          == 0 = genCaptures id id
  | popCount allCheckers > 1 = allKingCaptures
  | sliderCheckers /= 0       = genCaptures captureChecker pawnCaptureChecker
  | otherwise                = genCaptures captureChecker pawnCaptureChecker

  where
    genCaptures =
      allCapturesHelper allPieces king allKingCaptures pos
    pawnCaptureChecker board =
      board & (allCheckers .| enPassantChecker)
    captureChecker board = board & allCheckers
    allKingCaptures =
      foldBoardSquares King (kingCaptures enemy attacked) [] kingSquare
    allCheckers = leapingCheckers .| sliderCheckers
    enPassantChecker =
      let checker = allCheckers & pawns
      in enPassant & (checker << 8 .| checker >> 8)
    allPieces  = player .| enemy
    kingSquare = lsb king
    king       = player&kings


allCapturesHelper :: Board -> Board -> [Move] -> Position -> (Board -> Board) -> (Board -> Board) -> [Move]
allCapturesHelper allPieces king allKingCaptures Position {..} !f !g =

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
    unpinned            = player .\ pinnedPieces
    pinnedPawns         = pinnedPieces & pawns
    filePinnedPawns     = pinnedPawns & kingFile
    diagPinnedPawns     = pinnedPawns & kingDiag
    antiDiagPinnedPawns = pinnedPawns & kingAntiDiag
    kingFile            = rankMovesVec !! kingSquare
    kingDiag            = antiDiagMovesVec !! kingSquare
    kingAntiDiag        = diagMovesVec !! kingSquare
    kingSquare          = lsb king
    noPieces            = (~) allPieces


staticExchangeCaptures :: Square -> Position -> [Move]
staticExchangeCaptures target pos@Position {..}
  | allCheckers == 0               = genCaptures
  | popCount allCheckers > 1      = allKingCaptures
  | testSquare allCheckers target = genCaptures
  | otherwise                     = []

  where
    genCaptures =
      staticExchangeCapturesHelper target kingSquare allPieces
                                   allKingCaptures pos
    allKingCaptures =
      foldBoardSquares King (kingCaptures (toBoard target) attacked)
      [] kingSquare
    allCheckers     = leapingCheckers .| sliderCheckers
    allPieces       = player .| enemy
    kingSquare      = lsb king
    king            = player&kings


staticExchangeCapturesHelper
  :: Square -> Square -> Board -> [Move] -> Position -> [Move]
staticExchangeCapturesHelper
  target king allPieces allKingCaptures Position {..} =
      foldBoardPawnMovesConst    target (validSliders&pawnAttackers)
    $ foldBoardMovesConst Knight target (unpinned&knightAttackers)
    $ foldBoardMovesConst Bishop target (validSliders&bishopAttackers)
    $ foldBoardMovesConst Rook   target (validSliders&rookAttackers)
    $ foldBoardMovesConst Queen  target (validSliders&queenAttackers)
      allKingCaptures

    where
    pawnAttackers   = pawns & pawnAttacks (reverseColor color) targetBoard
    knightAttackers = knights & knightAttacks target
    bishopAttackers = bishops & bishopRays
    rookAttackers   = rooks & rookRays
    queenAttackers  = queens & (bishopRays .| rookRays)
    bishopRays      = bishopAttacks allPieces target
    rookRays        = rookAttacks allPieces target
    targetBoard     = toBoard target
    unpinned        = player .\ pinnedPieces

    validSliders
      | pinnedPieces .\ knights == 0 = player
      | otherwise = unpinned .| pinnedPieces & getPinningRay target king


getPinningRay :: Square -> Square -> Board
getPinningRay !target !king
    | testSquare kingDiag target     = kingDiag
    | testSquare kingAntiDiag target = kingAntiDiag
    | testSquare kingFile target     = kingFile
    | testSquare kingRank target     = kingRank
    | otherwise                      = 0
  where
  kingDiag        = antiDiagMovesVec !! king
  kingAntiDiag    = diagMovesVec !! king
  kingFile        = rankMovesVec !! king
  kingRank        = fileMovesVec !! king


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
  (kingAttacks n & enemy) .\ attacked


bishopCaptures :: Board -> Board -> Board -> Board -> Square -> Board
bishopCaptures enemy allPieces pinnedPieces king n =
  bishopMoves allPieces pinnedPieces king n & enemy


rookCaptures :: Board -> Board -> Board -> Board -> Square -> Board
rookCaptures enemy allPieces pinnedPieces king n =
  rookMoves allPieces pinnedPieces king n & enemy


queenCaptures :: Board -> Board -> Board -> Board -> Square -> Board
queenCaptures enemy allPieces pinnedPieces king n =
  queenMoves allPieces pinnedPieces king n & enemy
