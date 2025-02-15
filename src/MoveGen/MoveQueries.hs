module MoveGen.MoveQueries (isQuietMove, isLegalQuietMove, isWinningCapture, isCheckMove, isPromotionPush, isCastlingMove, isCapture) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceQuietMoves (allQuietMoves)
import           MoveGen.PositionQueries
import           Utils.Board


isCastlingMove :: Move -> Bool
isCastlingMove Move {..} =
  piece == King && abs (start - end) == 2


isCheckMove :: Move -> Position -> Bool
isCheckMove mv pos =
  isKingInCheck $ makeMove mv pos


isCapture :: Move -> Position -> Bool
isCapture Move {..} Position {..} =
  promotion /= NoProm
    || testSquare enemy end
    || piece == Pawn && testSquare enPassant end


isWinningCapture :: Move -> Position -> Bool
isWinningCapture mv pos =
  isCapture mv pos
    && member mv.promotion bestPromotions
    && evaluateExchange mv pos >= 0


isPromotionPush :: Move -> Bool
isPromotionPush Move{..} =
  piece == Pawn && toRank end `elem` [2, 7]


isQuietMove :: Move -> Position -> Bool
isQuietMove mv pos =
  not (isCheckMove mv pos || isCapture mv pos)


isLegalQuietMove :: Move -> Position -> Bool
isLegalQuietMove mv@Move {..} pos@Position {..} =
  testSquare player start
    && not (testSquare allPieces end)
    && isPieceAt piece start pos
    && elem mv (allQuietMoves pos)
  where
    allPieces = enemy .| player
