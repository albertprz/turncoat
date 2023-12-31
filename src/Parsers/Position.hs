module Parsers.Position where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Parsers.Char      (space)
import           Bookhound.Parsers.Number    (unsignedInt)
import           Data.Char                   (digitToInt)

import           Constants.Boards
import           Models.Piece
import           Models.Position
import           MoveGen.MakeMove


positionFromFen :: Text -> Either [ParseError] Position
positionFromFen = runParser positionFenParser

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
    <$> (piecesP <* space)
    <*> (colorP <* space)
    <*> (castlingP <* space)
    <*> (enPassantP <* space)
    <*> (unsignedInt <* space)
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
  mandatory = (=<<) (fromMaybe empty . map pure)
  foldrFlipped f xs start = foldr f start xs


squareParser :: Parser Square
squareParser = (+) <$> column <*> map (* 8) row
  where
    column = (\x -> x - fromEnum 'a') . fromEnum <$> oneOf ['a' .. 'h']
    row = (\x -> x - 1) . digitToInt <$> oneOf ['1' .. '8']
