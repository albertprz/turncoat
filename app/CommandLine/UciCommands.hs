module CommandLine.UciCommands where

import           AppPrelude

import           Evaluation.Evaluation
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
import           Data.Map                  (traverseWithKey)
import           System.TimeIt


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Search opts     -> printBestMove opts
  Perft n         -> printPerft    n
  Divide n        -> printDivide   n
  SetPosition pos -> setPosition   pos
  MakeMove mv     -> move          mv
  Evaluate        -> printEvaluate


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
    go = (=<<) putStrLn
         . map tshow
         . timeItPure
         .: perft


printDivide :: Depth -> CommandM ()
printDivide = withPosition go
  where
    go = void
         . traverseWithKey (\k v -> putStrLn (tshow k <> ": " <> tshow v))
         .: divide


printEvaluate :: CommandM ()
printEvaluate = withPosition (const go) ()
  where
    go = putStrLn
         . ("\n" <>)
         . tshow
         . getScoreBreakdown


setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  updatePosition
    $ foldM (flip makeUnknownMove) initialPosition moves


move :: UnknownMove -> CommandM ()
move mv =
  updatePosition
    . makeUnknownMove mv =<< get
  

updatePosition :: Maybe Position -> CommandM ()
updatePosition = \case
  Just pos -> put pos
  Nothing  -> putStrLn "Error: Invalid Position"

  
  
makeUnknownMove :: UnknownMove -> Position -> Maybe Position
makeUnknownMove UnknownMove {..} pos =
  (`makeMove` pos) <$> mv
  where
  mv = find (\x -> x.start == start && x.end == end)
            (allMoves pos)


withPosition :: MonadState Position m => (a -> Position -> m b) -> a -> m b
withPosition f n = 
  f n =<< get


timeItPure :: MonadIO m => a -> m a
timeItPure x = timeIt do
  !result <- pure x
  pure result
  

type CommandM = StateT Position IO
