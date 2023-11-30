module Models.Piece where

import           ClassyPrelude


data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Enum, Bounded, Show)

data Promotion = KnightProm | BishopProm | RookProm | QueenProm
  deriving (Eq, Ord, Enum, Bounded, Show)

data Color = White | Black
  deriving (Eq, Ord, Enum, Bounded, Show)


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
