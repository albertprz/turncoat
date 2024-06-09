module Models.Command where

import           AppPrelude

import           Models.Position
import           Models.Score
import           Utils.Board


data Command
  = Uci
  | UciNewGame
  | IsReady
  | Search SearchOptions
  | Perft Depth
  | Divide Depth
  | SetPosition PositionSpec
  | SetOption OptionSpec
  | MakeMove UnknownMove
  | Stop
  | Quit
  | Evaluate
  | Display
  | Flip


data EngineInfo = EngineInfo
  { name    :: Text
  , version :: Text
  , author  :: Text
  }

data EngineState = EngineState
  { position :: Position
  , options  :: EngineOptions
  }

newtype EngineOptions = EngineOptions
  { hashSize :: Int
  }

data EngineOption = SpinOption
  { deflt :: Int
  , lo    :: Int
  , hi    :: Int
  }

newtype OptionSpec = HashSize Int

data SearchOptions = SearchOptions
  { searchMoves        :: [UnknownMove]
  , ponder             :: Bool
  , whiteTime          :: Int
  , whiteIncrement     :: Int
  , blackTime          :: Int
  , blackIncrement     :: Int
  , movesUntilNextTime :: Int
  , depth              :: Depth
  , nodes              :: Int
  , findMate           :: Int
  , moveTime           :: Int
  , infinite           :: Bool
  }


data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }


data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }


engineInfo :: EngineInfo
engineInfo = EngineInfo
  { name    = "Apostate"
  , version = "0.1.0"
  , author  = "Alberto Perez"
  }


initialEngineState :: EngineState
initialEngineState = EngineState
  { position = startPosition
  , options  = defaultEngineOptions
  }


defaultEngineOptions :: EngineOptions
defaultEngineOptions = EngineOptions
  { hashSize = 1024
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
  { searchMoves        = []
  , infinite           = True
  , ponder             = False
  , findMate           = 0
  , whiteTime          = 0
  , whiteIncrement     = 0
  , blackTime          = 0
  , blackIncrement     = 0
  , movesUntilNextTime = 0
  , depth              = 0
  , nodes              = 0
  , moveTime           = 0
  }


instance Show EngineOption where
  show = \case
    SpinOption {..} -> "type spin" <> " default " <> show deflt
                <> " min " <> show lo <> " max " <> show hi
