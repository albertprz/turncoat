module Models.GameState where


import           Models.Position (Position)


newtype GameState = GameState {
  positions :: [Position]
}
