module Models.Command where

import           Models.Position
import           Models.Score
import           Utils.Board



data Command
  = Search SearchOptions
  | Perft Depth
  | Divide Depth
  | SetPosition PositionSpec
  | MakeMove UnknownMove
  | Evaluate


newtype SearchOptions = SearchOptions
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
defaultSearchOptions = SearchOptions
  { depth = 12
  }
