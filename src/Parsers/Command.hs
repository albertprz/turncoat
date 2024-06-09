module Parsers.Command (parseCommand) where

import           AppPrelude

import           Models.Command
import           Models.Position
import           Models.Score
import           Parsers.Position

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Parsers.Number
import           Bookhound.Parsers.Text
import           Control.Newtype
import           Data.Monoid


parseCommand :: Text -> Either [ParseError] Command

parseCommand = runParser command
  where
  command =
        stringToken "uci"        $> Uci
    <|> stringToken "ucinewgame" $> UciNewGame
    <|> stringToken "isready"    $> IsReady
    <|> stringToken "position"   *> (SetPosition <$> positionSpec)
    <|> stringToken "setoption"  *> (SetOption   <$> optionSpec)
    <|> stringToken "move"       *> (MakeMove    <$> unknownMove)
    <|> stringToken "debug"      *> (Debug       <$> boolean)
    <|> stringToken "quit"       $> Quit
    <|> stringToken "eval"       $> Evaluate
    <|> stringToken "display"    $> Display
    <|> stringToken "flip"       $> Flip
    <|> stringToken "go" *> (
          stringToken "perft"    *> (Perft  <$> depth)
      <|> stringToken "divide"   *> (Divide <$> depth)
      <|> (Search <$> searchOptions)
    )
  positionSpec = PositionSpec
    <$> initialPosition
    <*> (stringToken "moves" *> (token unknownMove |+) <|> pure [])
  optionSpec = stringToken "name" *>
    ((stringToken "Hash" $> HashSize) <*> (stringToken "value" *> int))

  searchOptions =
    ($ defaultSearchOptions) . ala Endo foldMap <$> (searchOption |*)
  searchOption =
    includeDepth <$> (stringToken "depth" *> depth)
  initialPosition =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen"  *> positionFenParser
  unknownMove = UnknownMove <$> squareParser <*> squareParser
  token = withTransform maybeBetweenSpacing
  stringToken = token . string
  depth = fromIntegral <$> satisfy (inRange 1 20) unsignedInt
  boolean = stringToken "on" $> True
        <|> stringToken "off" $> False


includeDepth :: Depth -> SearchOptions -> SearchOptions
includeDepth depth opts = opts {
  depth = depth
  }
