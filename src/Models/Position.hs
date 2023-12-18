module Models.Position where


import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Parsers.Char      (space)
import           Bookhound.Parsers.Number    (unsignedInt)
import           ClassyPrelude
import           Constants.Boards
import           Data.Bitraversable          (bisequence)
import           Data.Bits                   (Bits (..))
import           Data.Char                   (digitToInt)
import           Data.List.Split             (chunksOf)
import           Models.Piece


data Position = Position {
  color           :: Color,
  attacked        :: Board,
  castling        :: Board,
  enPassant       :: Board,
  leapingCheckers :: Board,
  sliderCheckers  :: Board,
  pinnedPieces    :: Board,
  player          :: Board,
  enemy           :: Board,
  pawns           :: Board,
  knights         :: Board,
  bishops         :: Board,
  rooks           :: Board,
  queens          :: Board,
  kings           :: Board
}

startPosition :: Position
startPosition = Position {
  color = White
  , castling = (rank_1 .| rank_8) & (file_A .| file_E .| file_H)
  , attacked = 0
  , enPassant = 0
  , leapingCheckers = 0
  , sliderCheckers = 0
  , pinnedPieces = 0
  , player = rank_1 .| rank_2
  , enemy = rank_7 .| rank_8
  , pawns = rank_2 .| rank_7
  , rooks = (rank_1 .| rank_8) & (file_A .| file_H)
  , knights = (rank_1 .| rank_8) & (file_B .| file_G)
  , bishops = (rank_1 .| rank_8) & (file_C .| file_F)
  , queens = (rank_1 .| rank_8) & file_D
  , kings = (rank_1 .| rank_8) & file_E
}

emptyPosition :: Position
emptyPosition = Position {
  color = White
  , castling = 0
  , attacked = 0
  , enPassant = 0
  , leapingCheckers = 0
  , sliderCheckers = 0
  , pinnedPieces = 0
  , player = 0
  , enemy = 0
  , pawns = 0
  , rooks = 0
  , knights = 0
  , bishops = 0
  , queens = 0
  , kings = 0
}

positionFromFen :: Text -> Either [ParseError] Position
positionFromFen fenStr = do
  (pieces, color, castling, enPassant, _, _) <- runParser positionP fenStr
  pure
    $ foldrFlipped includePiece pieces
    $ foldrFlipped includeCastling castling
    $ foldrFlipped includeEnPassant enPassant
    $ includeColor color emptyPosition
  where
  positionP = (,,,,,)
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
  lengthCheck :: [a] -> Bool
  lengthCheck = (== 8) . length
  mandatory = (=<<) (fromMaybe empty . map pure)
  foldrFlipped f xs start = foldr f start xs

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

includeColor :: Color -> Position -> Position
includeColor color pos = pos { color = color }

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
  pos {enPassant = toBoard square}

squareParser :: Parser Square
squareParser = (+) <$> column <*> map (* 8) row
  where
    column = (fromEnum 'a' -) . fromEnum <$> oneOf ['a' .. 'h']
    row = (1 -) . digitToInt <$> oneOf ['1' .. '8']


pieceAt :: Square -> Position -> Maybe (Piece, Color)
pieceAt n (Position {..}) = bisequence (piece, color')
  where
    piece  | testBit pawns n = Just Pawn
           | testBit knights n = Just Knight
           | testBit bishops n = Just Bishop
           | testBit rooks n = Just Rook
           | testBit queens n = Just Queen
           | testBit kings n = Just King
           | otherwise = Nothing

    color' | testBit player n = Just color
           | testBit enemy n = Just $ reverseColor color
           | otherwise = Nothing


instance Show Position where
  show pos =
    show pos.color <> " to play: \n\n" <> boardStr
    where
      boardStr = intercalate "\n" (intersperse ' ' <$> pieces)
      pieces = reverse $ chunksOf 8 do
        n <- toList squares
        pure $ maybe ' ' pieceToChar (pieceAt n pos)
