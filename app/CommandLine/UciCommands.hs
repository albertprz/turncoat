module CommandLine.UciCommands (executeCommand) where

import           AppPrelude

import           Models.Command
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           Search.Perft
import           Search.Search

import           Control.Monad.State
import           Data.Composition
import           Data.Map
import           System.TimeIt


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Search opts     -> printBestMove opts
  Perft n         -> printPerft    n
  Divide n        -> printDivide   n
  SetPosition pos -> setPosition   pos


printBestMove :: SearchOptions -> CommandM ()
printBestMove opts = do
  tTable       <- liftIO TTable.create
  killersTable <- liftIO KillersTable.create
  result       <- withPosition (go tTable killersTable) opts.depth
  liftIO $ TTable.clear       tTable
  liftIO $ KillersTable.clear killersTable
  pure result
  where
    printMove              = putStrLn . foldMap (("bestmove " ++) . tshow)
    go tTable killersTable = (printMove <=< liftIO . timeIt)
                             .: getBestMove
      where
        ?tTable       = tTable
        ?killersTable = killersTable


printPerft :: Depth -> CommandM ()
printPerft = withPosition go
  where
    go = putStrLn
         . tshow
         .: perft

printDivide :: Depth -> CommandM ()
printDivide = withPosition go
  where
    go = void
         . traverseWithKey (\k v -> putStrLn (tshow k <> ": " <> tshow v))
         .: divide


setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  case newPos of
    Just position -> put position
    Nothing       -> putStrLn "Error: Invalid Position"
  where
    newPos = foldM (flip makeUnknownMove) initialPosition moves


withPosition :: MonadState Position m => (a -> Position -> m b) -> a -> m b
withPosition f n = do
  f n =<< get


makeUnknownMove :: UnknownMove -> Position -> Maybe Position
makeUnknownMove UnknownMove {..} pos =
  (`makeMove` pos) <$> mv
  where
  mv = find (\x -> x.start == start && x.end == end)
            (allMoves pos)

type CommandM = StateT Position IO
