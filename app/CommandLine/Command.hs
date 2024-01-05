module CommandLine.Command where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators

import           Bookhound.Parsers.Number
import           Bookhound.Parsers.Text
import           CommandLine.UciCommands
import           Models.Position
import           Models.Score
import           Parsers.Position
import           Search.SearchOptions

import           Data.Monoid


data Command
  = SetPosition PositionSpec
  | Perft Depth
  | Divide Depth
  | Search SearchOptions


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
  depth = (Depth . fromIntegral) <$> unsignedInt


executeCommand :: Command -> CommandM ()
executeCommand = \case
  SetPosition pos -> setPosition pos
  Perft n -> printPerft n
  Divide n -> printDivide n
  Search opts -> printBestMove opts
