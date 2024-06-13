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
    <|> stringToken "ponderhit"  $> Ponderhit
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
    <|> setInfinite       <$   stringToken "infinite"
    <|> setPonder         <$> (stringToken "ponder"    $> True)
    <|> setWhiteTime      <$> (stringToken "wtime"     *> unsignedInt)
    <|> setBlackTime      <$> (stringToken "btime"     *> unsignedInt)
    <|> setWhiteIncrement <$> (stringToken "winc"      *> unsignedInt)
    <|> setBlackIncrement <$> (stringToken "binc"      *> unsignedInt)
    <|> setMovesUntil     <$> (stringToken "movestogo" *> unsignedInt)
    <|> setTargetDepth    <$> (stringToken "depth"     *> depth)
    <|> setNodes          <$> (stringToken "nodes"     *> unsignedInt)
    <|> setFindMate       <$> (stringToken "mate"      *> unsignedInt)
    <|> setMoveTime       <$> (stringToken "movetime"  *> unsignedInt)

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
  token       = withTransform maybeBetweenSpacing
  stringToken = token . string
  depth       = fromIntegral <$> satisfy (inRange 1 255) unsignedInt

  setSearchMoves x opts    = opts { searchMoves        = x }
  setPonder x opts         = opts { ponder             = x }
  setTargetDepth x opts    = opts { targetDepth        = x }
  setInfinite opts         = opts { targetDepth        = maxBound }
  setMoveTime x opts       = opts { moveTime           = Just x }
  setWhiteTime x opts      = opts { whiteTime          = Just x }
  setWhiteIncrement x opts = opts { whiteIncrement     = Just x }
  setBlackTime x opts      = opts { blackTime          = Just x }
  setBlackIncrement x opts = opts { blackIncrement     = Just x }
  setMovesUntil x opts     = opts { movesUntilNextTime = Just x }
  setNodes x opts          = opts { nodes              = Just x }
  setFindMate x opts       = opts { findMate           = Just x }
