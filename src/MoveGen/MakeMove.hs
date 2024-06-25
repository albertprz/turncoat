module MoveGen.MakeMove (makeMove, makeNullMove) where

import           AppPrelude

import           Evaluation.Material
import           Evaluation.Parameters
import           Models.Move
import           Models.Piece
import           Models.Position
import           MoveGen.PieceAttacks
import           Utils.Board


makeMove :: Move -> Position -> Position
makeMove Move {..} =
  makeNullMove
  . movePiece piece promotion startBoard endBoard
  . updatePlayerBoards startBoard endBoard end
  where
    !startBoard = toBoard start
    !endBoard = toBoard end


makeNullMove :: Position -> Position
makeNullMove pos@Position {materialScore, color, player, enemy, enPassant} =
  pos {
    materialScore   = - materialScore
  , color           = reverseColor color
  , phase           = getPhase pos
  , player          = enemy
  , enemy           = player
  , attacked        = allAttacks pos
  , enPassant       = toReverseCondition enPassantPinnedPawns * enPassant
  , leapingCheckers = getLeapingCheckers pos
  , sliderCheckers  = getSliderCheckers bishopCheckerRays
                        rookCheckerRays pos
  , pinnedPieces    = getPinnedPieces bishopCheckerRays
                        rookCheckerRays sliderRays pos
  }
  where
    bishopCheckerRays = getBishopCheckerRays pos
    rookCheckerRays   = getRookCheckerRays pos
    sliderRays        = getEnemyKingSliderRays pos
    enPassantPinnedPawns
      | enPassant == 0 = 0
      | otherwise     = getEnPassantPinnedPawns pos


updatePlayerBoards :: Board -> Board -> Square -> Position -> Position
updatePlayerBoards start end endSquare pos@Position {..} =
  pos {
      previousPositions    = getZobristKey pos
        : if halfMoveClock == 0 then [] else previousPositions
    , materialScore        = materialScore + materialDiff
    , halfMoveClock        = fromIntegral
        (toReverseCondition (enemy & end)) * (halfMoveClock + 1)
    , player               = (player ^ start) .| end
    , enemy                = enemy .\ end
    , pawns                = pawns .\ end
    , knights              = knights .\ end
    , bishops              = bishops .\ end
    , rooks                = rooks .\ end
    , queens               = queens .\ end
  }
  where
    !materialDiff =
      maybe 0 evaluateCapturedPiece (maybeCapturedPieceAt endSquare pos)


movePiece :: Piece -> Promotion -> Board -> Board -> Position -> Position
movePiece Pawn NoProm start end pos@Position {..} =
  pos {
      materialScore = materialScore
        + pawnScore * fromIntegral (popCount enPassantCapture)
    , halfMoveClock = 0
    , pawns         = (pawns ^ (start .| enPassantCapture)) .| end
    , enemy         = enemy ^ enPassantCapture
    , enPassant     = start <<! 8 & end !>> 8
  }
  where
    ((<<!), (!>>)) = case color of
      White -> ((<<), (>>))
      Black -> ((>>), (<<))
    enPassantCapture = (enPassant & end) !>> 8

movePiece Pawn KnightProm start end pos@Position {..} =
  pos {
      materialScore = materialScore + knightScore - pawnScore
    , halfMoveClock = 0
    , pawns = pawns ^ start
    , knights = knights .| end
    , enPassant = 0
  }

movePiece Pawn BishopProm start end pos@Position {..} =
  pos {
      materialScore = materialScore + bishopScore - pawnScore
    , halfMoveClock = 0
    , pawns = pawns ^ start
    , bishops = bishops .| end
    , enPassant = 0
  }

movePiece Pawn RookProm !start !end pos@Position {..} =
  pos {
      materialScore = materialScore + rookScore - pawnScore
    , halfMoveClock = 0
    , pawns = pawns ^ start
    , rooks = rooks .| end
    , enPassant = 0
  }

movePiece Pawn QueenProm start end pos@Position {..} =
  pos {
      materialScore = materialScore + queenScore - pawnScore
    , halfMoveClock = 0
    , pawns = pawns ^ start
    , queens = queens .| end
    , enPassant = 0
  }

movePiece Knight _ start end pos@Position {..} =
  pos {
      knights = (knights ^ start) .| end
    , enPassant = 0
  }

movePiece Bishop _ start end pos@Position {..} =
  pos {
      bishops = (bishops ^ start) .| end
    , enPassant = 0
  }

movePiece Rook _ start end pos@Position {..} =
  pos {
      rooks = (rooks ^ start) .| end
    , castling = castling .\ start
    , enPassant = 0
  }

movePiece Queen _ start end pos@Position {..} =
  pos {
      queens = (queens ^ start) .| end
    , enPassant = 0
  }

movePiece King _ start end pos@Position {..} =
  pos {
    kings = (kings ^ start) .| end
    , castling = castling .\ kingRank
    , player = (player ^ rookStart) .| rookEnd
    , rooks = (rooks ^ rookStart) .| rookEnd
    , enPassant = 0
  }
  where
    rookStart = shortCastle << 1 .| longCastle >> 2
    rookEnd = shortCastle >> 1 .| longCastle << 1
    shortCastle = (start << 2) & end
    longCastle = (start >> 2) & end
    kingRank = fileMovesVec !! lsb start
