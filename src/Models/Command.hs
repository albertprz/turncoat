module Models.Command where

import           AppPrelude

import           Models.Position
import           Models.Score
import           Utils.Board

import           Data.Word


data Command
  = Uci
  | UciNewGame
  | IsReady
  | Search SearchOptions
  | SetPosition PositionSpec
  | SetOption OptionSpec
  | Ponderhit
  | Stop
  | Quit
  | MakeMoves [UnknownMove]
  | Perft Depth
  | Divide Depth
  | Evaluate
  | Display
  | Flip


data EngineInfo = EngineInfo
  { name    :: Text
  , version :: Text
  , author  :: Text
  }

data EngineOptions = EngineOptions
  { hashSize :: Word16
  , ponder   :: Bool
  }

data EngineOption = SpinOption
  { deflt :: Word16
  , lo    :: Word16
  , hi    :: Word16
  }
  | CheckOption Bool
  | ButtonOption


data OptionSpec
  = HashSize Int
  | Ponder Bool
  | ClearHash

data SearchOptions = SearchOptions
  { searchMoves        :: [UnknownMove]
  , infinite           :: Bool
  , targetDepth        :: Depth
  , moveTime           :: Maybe Int
  , whiteTime          :: Maybe Int
  , whiteIncrement     :: Maybe Int
  , blackTime          :: Maybe Int
  , blackIncrement     :: Maybe Int
  , movesUntilNextTime :: Maybe Int
  , nodes              :: Maybe Int
  , findMate           :: Maybe Int
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
  { name    = "Turncoat"
  , version = "1.0"
  , author  = "Alberto Perez"
  }


defaultEngineOptions :: EngineOptions
defaultEngineOptions = EngineOptions
  { hashSize = 16
  , ponder   = False
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions
  { searchMoves        = []
  , infinite           = False
  , targetDepth        = maxBound
  , findMate           = Nothing
  , whiteTime          = Nothing
  , whiteIncrement     = Nothing
  , blackTime          = Nothing
  , blackIncrement     = Nothing
  , movesUntilNextTime = Nothing
  , nodes              = Nothing
  , moveTime           = Nothing
  }


instance Show EngineOption where
  show = \case
    SpinOption {..}    -> "type spin" <> " default " <> show deflt
                        <> " min " <> show lo <> " max " <> show hi
    CheckOption deflt -> "type check" <> " default " <> toLower (show deflt)
    ButtonOption      -> "type button"


type Task = Async ()
