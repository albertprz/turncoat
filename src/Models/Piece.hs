module Models.Piece where

import           ClassyPrelude


newtype Piece = Piece Word8
  deriving (Eq, Ord, Enum, Bounded,  Generic)

instance Show Piece where
  show = \case
    Pawn -> "Pawn"
    Knight -> "Knight"
    Bishop -> "Bishop"
    Rook -> "Rook"
    Queen -> "Queen"
    King -> "King"

instance Hashable Piece

newtype Promotion = Promotion Word8
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show Promotion where
  show = \case
    KnightProm -> "Knight"
    BishopProm -> "Bishop"
    RookProm -> "Rook"
    QueenProm -> "Queen"

instance Hashable Promotion

newtype Color = Color Word8
  deriving (Eq, Ord, Enum, Bounded)

instance Show Color where
  show = \case
    White -> "White"
    Black -> "Black"


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
pattern KnightProm = Promotion 0
pattern BishopProm = Promotion 1
pattern RookProm   = Promotion 2
pattern QueenProm  = Promotion 3

{-# COMPLETE White, Black #-}
pattern White, Black :: Color
pattern White = Color 0
pattern Black = Color 1


reverseColor :: Color -> Color
reverseColor = \case
  White -> Black
  Black -> White

pieceVal :: Piece -> Int
pieceVal = \case
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Rook   -> 5
  Queen  -> 9
  King   -> maxBound
