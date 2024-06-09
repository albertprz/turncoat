module Parsers.Command (parseCommand) where

import           AppPrelude

import           Models.Command
import           Models.Position
import           Parsers.Position

import           Bookhound.Parser
import           Bookhound.ParserCombinators
import           Bookhound.Parsers.Char      (space)
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
    <|> stringToken "stop"       $> Stop
    <|> stringToken "quit"       $> Quit
    <|> stringToken "eval"       $> Evaluate
    <|> stringToken "display"    $> Display
    <|> stringToken "flip"       $> Flip
    <|> stringToken "go" *>
         (stringToken "perft"    *> (Perft  <$> depth)
      <|> stringToken "divide"   *> (Divide <$> depth)
      <|> (Search <$> searchOptions))

  searchOption =
        setSearchMoves    <$> (stringToken "searchmoves" *>
                                someSepBy space unknownMove)
    <|> setInfinite       <$> (stringToken "infinite"  $> True)
    <|> setPonder         <$> (stringToken "ponder"    $> True)
    <|> setWhiteTime      <$> (stringToken "whitetime" *> unsignedInt)
    <|> setWhiteIncrement <$> (stringToken "whiteinc"  *> unsignedInt)
    <|> setBlackTime      <$> (stringToken "blacktime" *> unsignedInt)
    <|> setBlackIncrement <$> (stringToken "blackinc"  *> unsignedInt)
    <|> setDepth          <$> (stringToken "depth"     *> depth)
    <|> setMovesUntil     <$> (stringToken "movestogo" *> unsignedInt)
    <|> setNodes          <$> (stringToken "nodes"     *> unsignedInt)
    <|> setFindMate       <$> (stringToken "mate"      *> unsignedInt)
    <|> setMoveTime       <$> (stringToken "moveTime"  *> unsignedInt)

  positionSpec = PositionSpec
    <$> initialPositionSpec
    <*> (stringToken "moves" *> (token unknownMove |+) <|> pure [])

  optionSpec = stringToken "name" *>
    ((stringToken "Hash" $> HashSize) <*>
     (stringToken "value" *> unsignedInt))

  searchOptions =
    ($ defaultSearchOptions) . ala Endo foldMap <$> (searchOption |*)

  initialPositionSpec =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen"  *> positionFenParser

  unknownMove = UnknownMove <$> squareParser <*> squareParser
  token = withTransform maybeBetweenSpacing
  stringToken = token . string
  depth = fromIntegral <$> satisfy (inRange 1 255) unsignedInt

  setSearchMoves x opts = opts { searchMoves = x }
  setPonder x opts = opts { ponder = x }
  setWhiteTime x opts = opts { whiteTime = x }
  setWhiteIncrement x opts = opts { whiteIncrement = x }
  setBlackTime x opts = opts { blackTime = x }
  setBlackIncrement x opts = opts { blackIncrement = x }
  setMovesUntil x opts = opts { movesUntilNextTime = x }
  setDepth x opts = opts { depth = x }
  setNodes x opts = opts { nodes = x }
  setFindMate x opts = opts { findMate = x }
  setMoveTime x opts = opts { moveTime = x }
  setInfinite x opts = opts { infinite = x }
