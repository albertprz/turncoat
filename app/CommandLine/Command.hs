module CommandLine.Command where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators

import           Bookhound.Parsers.Number
import           Bookhound.Parsers.Text
import           CommandLine.UciCommands
import           Models.Position
import Search.SearchOptions
import           Parsers.Position

import Data.Monoid


data Command
  = SetPosition PositionSpec
  | Perft Int
  | Divide Int
  | BestMove SearchOptions


parseCommand :: Text -> Either [ParseError] Command
parseCommand = runParser command
  where
  command =
    stringToken "position" *> (SetPosition <$> positionSpec)
      <|> stringToken "go" *> (
          stringToken "perft"  *> (Perft  <$> unsignedInt)
      <|> stringToken "divide" *> (Divide <$> unsignedInt)
      <|> (BestMove <$> searchOptions)
    )
  positionSpec = PositionSpec
    <$> initialPosition
    <*> (stringToken "moves" *> (token unknownMove |+) <|> pure [])
  searchOptions =
    (`appEndo` defaultSearchOptions) . foldMap Endo <$> (searchOption |*)
  searchOption = includeDepth <$> (stringToken "depth" *> unsignedInt)
  initialPosition =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen" *> positionFenParser
  unknownMove = UnknownMove <$> squareParser <*> squareParser
  token = withTransform maybeBetweenSpacing
  stringToken = token . string


executeCommand :: Command -> CommandM ()
executeCommand = \case
  SetPosition pos -> setPosition pos
  Perft n -> printPerft n
  Divide n -> printDivide n
  BestMove opts -> printBestMove opts
