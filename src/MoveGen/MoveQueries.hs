module MoveGen.MoveQueries where

import           AppPrelude

import           Constants.Boards
import           Data.Bits                 (Bits (testBit))
import           Data.Composition
import           Evaluation.StaticExchange
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceAttacks


isEndgame :: Position -> Bool
isEndgame Position {..} =
  ones (player & allMinorPieces) < 3
    || ones (enemy & allMinorPieces) < 3
  where
    allMinorPieces = bishops .| knights .| rooks .| queens


isCheckOrWinningCapture :: Move -> Position -> Bool
isCheckOrWinningCapture mv pos =
  isCheckMove mv pos
    || isPromotionPush mv
    || isWinningCapture mv pos


isCastlingMove :: Move -> Bool
isCastlingMove Move {..} =
  piece == King && abs (start - end) == 2


isCheckMove :: Move -> Position -> Bool
isCheckMove mv pos =
  isKingInCheck $ makeMove mv pos


isCapture :: Move -> Position -> Bool
isCapture Move {..} Position {..} =
  isJust promotion
    || testBit (enemy .| enPassant) end

isWinningCapture :: Move -> Position -> Bool
isWinningCapture mv pos =
  isCapture mv pos
    && evaluateCaptureExchange mv pos >= 0

isPromotionPush :: Move -> Bool
isPromotionPush Move{..} =
  piece == Pawn && toRank end `elem` [2, 7]


isQuietMove :: Move -> Position -> Bool
isQuietMove = not .: isCapture


isKingInCheck :: Position -> Bool
isKingInCheck Position {..} =
  sliderCheckers .| leapingCheckers /= 0


isEnemyKingInCheck :: Position -> Bool
isEnemyKingInCheck pos@Position {..} =
  player & potentialCheckers /= 0
  where
    potentialCheckers =
      pawns & pawnAttacks (reverseColor color) (enemy&kings)
      .| knights & knightAttacks (lsb (enemy&kings))
      .| kings & kingAttacks (lsb (enemy&kings))
      .| bishopCheckerRays & bishops
      .| rookCheckerRays   & rooks
      .| queenCheckerRays  & queens
    bishopCheckerRays = getBishopCheckerRays pos
    rookCheckerRays = getRookCheckerRays pos
    queenCheckerRays = bishopCheckerRays .| rookCheckerRays


isLegalQuietMove :: Move -> Position -> Bool
isLegalQuietMove mv@Move {..} pos@Position {..} =
  testBit player start
    && not (testBit allPieces end)
    && not (isCastlingMove mv)
    && isPieceAt piece start pos
    && isRayUnblocked allPieces mv
    && not (isEnemyKingInCheck $ quietMakeMove mv pos)
  where
    allPieces = enemy .| player


isRayUnblocked :: Board -> Move -> Bool
isRayUnblocked allPieces Move {..} =
 piece `elem` [Knight, King]
   || testBit (queenAttacks allPieces start) end


quietMakeMove :: Move -> Position -> Position
quietMakeMove Move {..} pos = newPos {
    player = newPos.enemy,
    enemy = newPos.player,
    color = reverseColor newPos.color
  }
  where
    newPos = quietMovePiece piece startBoard endBoard pos
    startBoard = toBoard start
    endBoard = toBoard end


quietMovePiece :: Piece -> Board -> Board -> Position -> Position
quietMovePiece Pawn start end pos@Position {..} =
  pos {
    pawns = (pawns ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Knight start end pos@Position {..} =
  pos {
    knights = (knights ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Bishop start end pos@Position {..} =
  pos {
    bishops = (bishops ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Rook start end pos@Position {..} =
  pos {
    rooks = (rooks ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece Queen start end pos@Position {..} =
  pos {
    queens = (queens ^ start) .| end,
    player = (player ^ start) .| end
  }

quietMovePiece King start end pos@Position {..} =
  pos {
    kings = (kings ^ start) .| end,
    player = (player ^ start) .| end
  }
