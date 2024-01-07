module Models.Piece where

import           AppPrelude
import           Data.Char                 (isUpper)
import qualified Data.Char                 as Char
import           Foreign.Storable.Generic
import           Test.QuickCheck           (Arbitrary, chooseEnum)
import           Test.QuickCheck.Arbitrary (Arbitrary (..))


newtype Piece = Piece Word8
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Piece where
  arbitrary = chooseEnum (Pawn, King)

instance Hashable Piece
instance GStorable Piece

newtype Promotion = Promotion Word8
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Promotion where
  arbitrary = chooseEnum (KnightProm, QueenProm)

instance Hashable Promotion
instance GStorable Promotion

newtype Color = Color Word8
  deriving (Eq, Ord, Enum, Bounded)

data CastlingRights = KingSide | QueenSide


{-# COMPLETE Pawn, Knight, Bishop, Rook, Queen, King #-}
pattern Pawn, Knight, Bishop, Rook, Queen, King :: Piece
pattern Pawn   = Piece 0
pattern Knight = Piece 1
pattern Bishop = Piece 2
pattern Rook   = Piece 3
pattern Queen  = Piece 4
pattern King   = Piece 5

{-# COMPLETE  KnightProm, BishopProm, RookProm, QueenProm #-}
pattern KnightProm, BishopProm, RookProm, QueenProm :: Promotion
pattern KnightProm = Promotion 1
pattern BishopProm = Promotion 2
pattern RookProm   = Promotion 3
pattern QueenProm  = Promotion 4

{-# COMPLETE White, Black #-}
pattern White, Black :: Color
pattern White = Color 0
pattern Black = Color 1


reverseColor :: Color -> Color
reverseColor (Color color) =
  Color (1 - color)

pieceToChar :: (Piece, Color) -> Char
pieceToChar (piece, color) =
  case color of
    White -> Char.toUpper char
    Black -> char
  where
    char = case piece of
      Pawn   -> 'p'
      Knight -> 'n'
      Bishop -> 'b'
      Rook   -> 'r'
      Queen  -> 'q'
      King   -> 'k'

charToPiece :: Char -> Maybe (Piece, Color)
charToPiece char = (,color) <$> piece
  where
    color | isUpper char = White
          | otherwise    = Black
    piece = case Char.toLower char of
      'p' -> Just Pawn
      'n' -> Just Knight
      'b' -> Just Bishop
      'r' -> Just Rook
      'q' -> Just Queen
      'k' -> Just King
      _   -> Nothing

charToColor :: Char -> Maybe Color
charToColor char = case char of
  'w' -> Just White
  'b' -> Just Black
  _   -> Nothing

charToCastlingRights :: Char -> Maybe (CastlingRights, Color)
charToCastlingRights char = (,color) <$> piece
  where
    color | isUpper char = White
          | otherwise    = Black
    piece = case Char.toLower char of
      'k' -> Just KingSide
      'q' -> Just QueenSide
      _   -> Nothing

instance Show Piece where
  show = \case
    Pawn -> "Pawn"
    Knight -> "Knight"
    Bishop -> "Bishop"
    Rook -> "Rook"
    Queen -> "Queen"
    King -> "King"

instance Show Promotion where
  show = \case
    KnightProm -> "n"
    BishopProm -> "b"
    RookProm -> "r"
    QueenProm -> "q"

instance Show Color where
  show = \case
    White -> "White"
    Black -> "Black"
