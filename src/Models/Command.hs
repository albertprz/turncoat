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
  | Debug Bool
  | MakeMove UnknownMove
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
  { depth = 12
  }


instance Show EngineOption where
  show = \case
    SpinOption {..} -> "type spin" <> " default " <> show deflt
                <> " min " <> show lo <> " max " <> show hi
