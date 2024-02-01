module Models.Command where

import           Constants.Boards
import           Models.Position
import           Models.Score


data Command
  = Search SearchOptions
  | Perft Depth
  | Divide Depth
  | SetPosition PositionSpec


data SearchOptions = SearchOptions
  { depth :: Depth
  }


data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }


data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions {
  depth = 12
}
