module Models.GameState where

import           ClassyPrelude

import           Models.Position (Position)


data GameState = GameState {
  ply       :: Ply,
  positions :: [Position]
}

type Ply = Int
