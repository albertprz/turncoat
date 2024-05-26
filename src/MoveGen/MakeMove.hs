module MoveGen.MakeMove (makeMove, makeNullMove, switchPlayers) where

import           AppPrelude

import           Constants.Boards
import           Evaluation.Material
import           Evaluation.PieceTables
import           Models.Move
import           Models.Piece
import           Models.Position
import qualified MoveGen.PieceAttacks   as MoveGen


makeMove :: Move -> Position -> Position
makeMove Move {..} =
  switchPlayers
  . movePiece piece promotion startBoard endBoard
  . updatePlayerBoards startBoard endBoard end
  where
    startBoard = toBoard start
    endBoard = toBoard end


makeNullMove :: Position -> Position
makeNullMove = switchPlayers


switchPlayers :: Position -> Position
switchPlayers pos@Position {..} =
  pos {
    materialScore   = - materialScore
  , color           = reverseColor color
  , player          = enemy
  , enemy           = player
  , attacked        = MoveGen.allPlayerAttacks pos
  , leapingCheckers = MoveGen.getLeapingCheckers pos
  , sliderCheckers  = MoveGen.getSliderCheckers bishopCheckerRays
                        rookCheckerRays queenCheckerRays pos
  , pinnedPieces    = MoveGen.getPinnedPieces bishopCheckerRays
                        rookCheckerRays sliderRays pos
  , enPassant       = toEnum (1 - ones enPassantPinnedPawns)
                        * enPassant
  }
  where
    bishopCheckerRays = MoveGen.getBishopCheckerRays pos
    rookCheckerRays = MoveGen.getRookCheckerRays pos
    queenCheckerRays = bishopCheckerRays .| rookCheckerRays
    sliderRays = MoveGen.getEnemyKingSliderRays pos
    enPassantPinnedPawns
      | enPassant == 0 = 0
      | otherwise = MoveGen.getEnPassantPinnedPawns pos


updatePlayerBoards :: Board -> Board -> Square -> Position -> Position
updatePlayerBoards start end endSquare pos@Position {..} =
  pos {
    previousPositions = getZobristKey pos
      : if halfMoveClock == 0
        then []
        else previousPositions,
    materialScore = materialScore
      + maybe 0 (evaluateCapturedPiece color endSquare)
                (maybeCapturedPieceAt endSquare pos),
    halfMoveClock = fromIntegral
      (1 - ones (enemy & end)) * (halfMoveClock + 1),
    player = (player ^ start) .| end,
    enemy = enemy .\ end,
    pawns = pawns .\ end,
    knights = knights .\ end,
    bishops = bishops .\ end,
    rooks = rooks .\ end,
    queens = queens .\ end
  }


movePiece :: Piece -> Maybe Promotion -> Board -> Board -> Position -> Position
movePiece Pawn Nothing start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + pawnSquareTable !! endIdx
      - pawnSquareTable !! startIdx
      + fromIntegral (ones enPassantCapture)
      * (pawnScore + pawnSquareTable !! enPassantIdx),
    halfMoveClock = 0,
    pawns = (pawns ^ (start .| enPassantCapture)) .| end,
    enemy = enemy ^ enPassantCapture,
    enPassant = start <<! 8 & end !>> 8
  }
  where
    ((<<!), (!>>)) = case color of
      White -> ((<<), (>>))
      Black -> ((>>), (<<))
    enPassantCapture = (enPassant & end) !>> 8
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color
    enPassantIdx =
      getSquareTableIndex enPassantCapture (reverseColor color)

movePiece Pawn (Just KnightProm) start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + (knightScore + knightSquareTable !! endIdx)
      - (pawnScore + pawnSquareTable !! startIdx),
    halfMoveClock = 0,
    pawns = pawns ^ start,
    knights = knights .| end,
    enPassant = 0
  }
  where
    endIdx = lsb end + 64 * fromIntegral color
    startIdx = lsb start + 64 * fromIntegral color

movePiece Pawn (Just BishopProm) start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + (bishopScore + bishopSquareTable !! endIdx)
      - (pawnScore + pawnSquareTable !! startIdx),
    halfMoveClock = 0,
    pawns = pawns ^ start,
    bishops = bishops .| end,
    enPassant = 0
  }
  where
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color

movePiece Pawn (Just RookProm) start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + (rookScore + rookSquareTable !! endIdx)
      - (pawnScore + pawnSquareTable !! startIdx),
    halfMoveClock = 0,
    pawns = pawns ^ start,
    rooks = rooks .| end,
    enPassant = 0
  }
  where
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color

movePiece Pawn (Just QueenProm) start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + (queenScore + queenSquareTable !! endIdx)
      - (pawnScore + pawnSquareTable !! startIdx),
    halfMoveClock = 0,
    pawns = pawns ^ start,
    queens = queens .| end,
    enPassant = 0
  }
  where
    endIdx = lsb end + 64 * fromIntegral color
    startIdx = lsb start + 64 * fromIntegral color

movePiece Knight _ start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + knightSquareTable !! endIdx
      - knightSquareTable !! startIdx,
    knights = (knights ^ start) .| end,
    enPassant = 0
  }
  where
    endIdx = lsb end + 64 * fromIntegral color
    startIdx = lsb start + 64 * fromIntegral color

movePiece Bishop _ start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + bishopSquareTable !! endIdx
      - bishopSquareTable !! startIdx,
    bishops = (bishops ^ start) .| end,
    enPassant = 0
  }
  where
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color

movePiece Rook _ start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + rookSquareTable !! endIdx
      - rookSquareTable !! startIdx,
    rooks = (rooks ^ start) .| end,
    castling = castling .\ start,
    enPassant = 0
  }
  where
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color

movePiece Queen _ start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + queenSquareTable !! endIdx
      - queenSquareTable !! startIdx,
    queens = (queens ^ start) .| end,
    enPassant = 0
  }
  where
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color

movePiece King _ start end pos@Position {..} =
  pos {
    materialScore = materialScore
      + kingSquareTable !! endIdx
      - kingSquareTable !! startIdx
      + fromIntegral (ones rookStart)
      * (rookSquareTable !! rookEndIdx
        - rookSquareTable !! rookStartIdx),
    kings = (kings ^ start) .| end,
    castling = castling .\ kingRank,
    player = (player ^ rookStart) .| rookEnd,
    rooks = (rooks ^ rookStart) .| rookEnd,
    enPassant = 0
  }
  where
    rookStart = shortCastle << 1 .| longCastle >> 2
    rookEnd = shortCastle >> 1 .| longCastle << 1
    shortCastle = (start << 2) & end
    longCastle = (start >> 2) & end
    kingRank = fileMovesVec !! lsb start
    endIdx = getSquareTableIndex end color
    startIdx = getSquareTableIndex start color
    rookEndIdx = getSquareTableIndex rookEnd color
    rookStartIdx = getSquareTableIndex rookStart color
