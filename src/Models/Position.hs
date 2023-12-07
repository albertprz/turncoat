module Models.Position where


import           ClassyPrelude
import           Constants.Boards
import           Data.Bitraversable (bisequence)
import           Data.Bits          (Bits (..))
import qualified Data.Char          as Char
import           Data.List.Split    (chunksOf)
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

pieceToChar :: (Piece, Color) -> Char
pieceToChar (piece, color) =
  case color of
    White -> pieceChar
    Black -> Char.toLower pieceChar
  where
    pieceChar = case piece of
      Pawn   -> 'P'
      Knight -> 'N'
      Bishop -> 'B'
      Rook   -> 'R'
      Queen  -> 'Q'
      King   -> 'K'

pieceAt :: Square -> Position -> Maybe (Piece, Color)
pieceAt n (Position {..}) = bisequence (piece, color')
  where
    piece | testBit pawns n = Just Pawn
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
        pure $! maybe ' ' pieceToChar (pieceAt n pos)
