module Models.Command where

import           Constants.Boards
import           Models.Position
import           Models.Score


data Command
  = SetPosition PositionSpec
  | Perft Depth
  | Divide Depth
  | Search SearchOptions


data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }

data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }


data SearchOptions = SearchOptions
  { depth :: Depth
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions {
  depth = 12
}


includeDepth :: Depth -> SearchOptions -> SearchOptions
includeDepth depth opts = opts {
  depth = depth
  }
