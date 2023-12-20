module CommandLine.Command where

import           AppPrelude

import           Bookhound.Parser
import           Bookhound.ParserCombinators

import           Bookhound.Parsers.Number
import           Bookhound.Parsers.Text
import           CommandLine.UciCommands
import           Models.Position


data Command
  = SetPosition PositionSpec
  | Perft Int
  | Divide Int


parseCommand :: Text -> Either [ParseError] Command
parseCommand = runParser command
  where
  command =
    stringToken "position" *> (SetPosition <$> positionSpec)
      <|> stringToken "go" *> (
          stringToken "perft"  *> (Perft  <$> unsignedInt)
      <|> stringToken "divide" *> (Divide <$> unsignedInt)
    )
  positionSpec = PositionSpec
    <$> initialPosition
    <*> (stringToken "moves" *> (token unknownMove |+) <|> pure [])
  initialPosition =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen" *> positionFenParser
  unknownMove = UnknownMove <$> squareParser <*> squareParser
  token = withTransform maybeBetweenSpacing
  stringToken = token . string


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Perft n -> printPerft n
  Divide n -> printDivide n
  SetPosition pos -> setPosition pos
