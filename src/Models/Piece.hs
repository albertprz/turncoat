module Models.Piece where

import           ClassyPrelude


data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Ord, Enum, Bounded, Show)

data Color = White | Black
  deriving (Eq, Ord, Enum, Bounded, Show)

data Direction = NW | N | NE | W | E | SW | S | SE |
                NWW | NNW | NNE | NEE | SWW | SSW | SSE | SEE

pieceVal :: Piece -> Int
pieceVal = \case
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Rook   -> 5
  Queen  -> 9
  King   -> maxBound
