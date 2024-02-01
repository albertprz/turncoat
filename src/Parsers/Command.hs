module Parsers.Command where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators

import           Bookhound.Parsers.Number
import           Bookhound.Parsers.Text
import           Models.Command
import           Models.Position
import           Models.Score
import           Parsers.Position

import           Data.Monoid


parseCommand :: Text -> Either [ParseError] Command
parseCommand = runParser command
  where
  command =
    stringToken "position" *> (SetPosition <$> positionSpec)
      <|> stringToken "go" *> (
          stringToken "perft"  *> (Perft  <$> depth)
      <|> stringToken "divide" *> (Divide <$> depth)
      <|> (Search <$> searchOptions)
    )
  positionSpec = PositionSpec
    <$> initialPosition
    <*> (stringToken "moves" *> (token unknownMove |+) <|> pure [])
  searchOptions =
    (`appEndo` defaultSearchOptions) . foldMap Endo <$> (searchOption |*)
  searchOption =
    includeDepth <$> (stringToken "depth" *> depth)
  initialPosition =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen" *> positionFenParser
  unknownMove = UnknownMove <$> squareParser <*> squareParser
  token = withTransform maybeBetweenSpacing
  stringToken = token . string
  depth = Depth . fromIntegral <$> satisfy (inRange 1 20) unsignedInt


includeDepth :: Depth -> SearchOptions -> SearchOptions
includeDepth depth opts = opts {
  depth = depth
  }
