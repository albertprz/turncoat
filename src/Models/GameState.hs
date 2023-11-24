module Models.GameState where

import           ClassyPrelude

import           Models.Board  (Position)
import           Models.Piece  (Color)


data GameState = GameState {
  playerColor :: Color,
  ply         :: Ply,
  positions   :: [Position]
}

newtype Ply = Ply Int
