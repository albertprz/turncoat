module Parsers.Command (parseCommand) where

import           AppPrelude

import           Models.Command
import           Models.Position
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
        stringToken "ucinewgame" $> UciNewGame
    <|> stringToken "uci"        $> Uci
    <|> stringToken "isready"    $> IsReady
    <|> stringToken "position"   *> (SetPosition <$> positionSpec)
    <|> stringToken "setoption"  *> (SetOption   <$> optionSpec)
    <|> stringToken "move"       *> (MakeMoves   <$> unknownMoves)
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
        setInfinite       <$   stringToken "infinite"
    <|> setPonder         <$   stringToken "ponder"
    <|> setSearchMoves    <$> (stringToken "searchmoves" *> unknownMoves)
    <|> setTargetDepth    <$> (stringToken "depth"       *> depth)
    <|> setWhiteTime      <$> (stringToken "wtime"       *> unsignedInt)
    <|> setBlackTime      <$> (stringToken "btime"       *> unsignedInt)
    <|> setWhiteIncrement <$> (stringToken "winc"        *> unsignedInt)
    <|> setBlackIncrement <$> (stringToken "binc"        *> unsignedInt)
    <|> setMovesUntil     <$> (stringToken "movestogo"   *> unsignedInt)
    <|> setNodes          <$> (stringToken "nodes"       *> unsignedInt)
    <|> setFindMate       <$> (stringToken "mate"        *> unsignedInt)
    <|> setMoveTime       <$> (stringToken "movetime"    *> unsignedInt)

  positionSpec = PositionSpec
    <$> initialPositionSpec
    <*> (stringToken "moves" *> unknownMoves <|> pure [])

  optionSpec = stringToken "name" *>
    ((stringToken "Hash" $> HashSize)
     <*> (stringToken "value" *> unsignedInt)
     <|> (stringToken "Ponder" $> Ponder)
     <*> (stringToken "value" *> boolean)
     <|> stringToken "Clear Hash" $> ClearHash)

  searchOptions =
    ($ defaultSearchOptions) . ala Endo foldMap <$> (searchOption |*)

  initialPositionSpec =
    stringToken "startpos" $> startPosition
    <|> stringToken "fen"  *> positionFenParser

  unknownMoves = (token unknownMove |+)
  unknownMove  = UnknownMove <$> squareParser <*> squareParser
  token        = withTransform maybeBetweenSpacing
  stringToken  = token . string
  depth        = fromIntegral <$> satisfy (inRange 1 255) unsignedInt
  boolean      = stringToken "true"  $> True
            <|> stringToken "false" $> False

  setSearchMoves x opts    = opts { searchMoves        = x }
  setInfinite opts         = opts { infinite           = True }
  setPonder opts           = opts { infinite           = True }
  setTargetDepth x opts    = opts { targetDepth        = x }
  setMoveTime x opts       = opts { moveTime           = Just x }
  setWhiteTime x opts      = opts { whiteTime          = Just x }
  setWhiteIncrement x opts = opts { whiteIncrement     = Just x }
  setBlackTime x opts      = opts { blackTime          = Just x }
  setBlackIncrement x opts = opts { blackIncrement     = Just x }
  setMovesUntil x opts     = opts { movesUntilNextTime = Just x }
  setNodes x opts          = opts { nodes              = Just x }
  setFindMate x opts       = opts { findMate           = Just x }
