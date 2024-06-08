module Parsers.Position (positionFenParser, squareParser) where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Parsers.Char      (space)
import           Bookhound.Parsers.Number    (unsignedInt)
import           Data.Char                   (digitToInt)

import           Evaluation.Material         (evaluateMaterial)
import           Models.Piece
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           Utils.Board


positionFenParser :: Parser Position
positionFenParser = do
  (pieces, color, castling, enPassant, halfMoveClock, _) <- position
  pure
    $ newPosition
    $ foldrFlipped includePiece pieces
    $ foldrFlipped includeCastling castling
    $ foldrFlipped includeEnPassant enPassant
    $ includeHalfMoveClock halfMoveClock
    $ includeColor color
    emptyPosition
  where
  position = (,,,,,)
    <$> (piecesP    <* space)
    <*> (colorP     <* space)
    <*> (castlingP  <* space)
    <*> (enPassantP <* space)
    <*> (ply        <* space)
    <*> unsignedInt
  piecesP =
    map (mapMaybe (\(x, y) -> (x,) <$> y) . zip [0 ..] . fold . reverse)
    $ satisfy lengthCheck
    $ satisfy (all lengthCheck)
    $ manySepBy (is '/') (fold <$> ((emptySquaresN <|> piece) |+))
  colorP = mandatory (map charToColor anyChar)
  castlingP = (mandatory (map charToCastlingRights anyChar) |+)
    <|> [] <$ is '-'
  enPassantP = pure <$> squareParser
    <|> Nothing <$ is '-'
  emptySquaresN = (`replicate` Nothing) . digitToInt <$> oneOf ['1' .. '8']
  piece = pure . pure <$> mandatory (map charToPiece anyChar)
  lengthCheck xs = length xs == 8
  ply = fromIntegral <$> unsignedInt
  mandatory = (=<<) (fromMaybe empty . map pure)
  foldrFlipped f = flip $ foldr f

squareParser :: Parser Square
squareParser = (+) <$> column <*> map (* 8) row
  where
    column = (\x -> x - fromEnum 'a') . fromEnum <$> oneOf ['a' .. 'h']
    row = (\x -> x - 1) . digitToInt <$> oneOf ['1' .. '8']

newPosition :: Position -> Position
newPosition = setInitialValues . makeNullMove . makeNullMove
  where
  setInitialValues pos = pos {
      materialScore = evaluateMaterial pos
    , phase         = getPhase pos
  }
  getPhase Position {..} = min totalPhase
     (minorPiecePhase * fromIntegral (popCount (knights .| bishops))
    + rookPhase       * fromIntegral (popCount rooks)
    + queenPhase      * fromIntegral (popCount queens))


includePiece :: (Square, (Piece, Color)) -> Position -> Position
includePiece (square, (piece, pieceColor)) pos@Position {..} =
  if pieceColor == color then
    pos' { player = player .| board }
  else
    pos' { enemy = enemy .| board }
  where
  pos' = case piece of
    Pawn   -> pos { pawns = pawns .| board }
    Knight -> pos { knights = knights .| board }
    Bishop -> pos { bishops = bishops .| board }
    Rook   -> pos { rooks = rooks .| board }
    Queen  -> pos { queens = queens .| board }
    King   -> pos { kings = kings .| board }
  board = toBoard square

includeHalfMoveClock :: Ply -> Position -> Position
includeHalfMoveClock halfMoveClock pos =
  pos { halfMoveClock = halfMoveClock }

includeColor :: Color -> Position -> Position
includeColor color pos =
  pos { color = color }

includeCastling :: (CastlingRights, Color) -> Position -> Position
includeCastling (castlingRights, castlingColor) pos@Position {..} =
  pos { castling = castling .| row & (column .| file_E) }
  where
  row = case castlingColor of
    White -> rank_1
    Black -> rank_8
  column = case castlingRights of
    QueenSide -> file_A
    KingSide  -> file_H

includeEnPassant :: Square -> Position -> Position
includeEnPassant square pos =
  pos { enPassant = toBoard square }
