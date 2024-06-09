module CommandLine.UciCommands where

import           AppPrelude

import           Evaluation.Evaluation
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
import qualified Data.Map                  as Map
import           Models.Command 
import           System.Exit
import           System.TimeIt


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Uci              -> handleStart
  UciNewGame       -> handleNewGame
  IsReady          -> printReady
  Evaluate         -> printStaticEval
  Search opts      -> printSearch opts
  Perft n          -> printPerft    n
  Divide n         -> printDivide   n
  SetPosition pos  -> setPosition   pos
  SetOption option -> setOption   option
  Quit             -> liftIO exitSuccess
  MakeMove mv      -> move          mv
  Display          -> displayBoard
  Flip             -> flipPosition


printSearch :: SearchOptions -> CommandM ()
printSearch opts = do
  st           <- get
  let ?options = st.options
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
         . Map.traverseWithKey (\k v -> putStrLn (tshow k <> ": " <> tshow v))
         .: divide


printStaticEval :: CommandM ()
printStaticEval = withPosition (const go) ()
  where
    go = putStrLn
         . ("\n" <>)
         . tshow
         . getScoreBreakdown
  
  
handleNewGame :: CommandM ()
handleNewGame = pure ()
  

handleStart :: CommandM ()
handleStart = do
  printEngineInfo
  putStrLn mempty
  printEngineOptions
  printUciReady


setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  updatePosition
    $ foldM (flip makeUnknownMove) initialPosition moves

  
setOption :: OptionSpec -> CommandM ()
setOption = \case
  HashSize size -> modifyOptions \x -> x { hashSize = size }


printUciReady :: CommandM ()
printUciReady =
  putStrLn "uciok"


printReady :: CommandM ()
printReady =
  putStrLn "readyok"


printEngineInfo :: CommandM ()
printEngineInfo = do
  go "name"  (name <> " " <> version)
  go "author" author
  where
    EngineInfo {..} = engineInfo
    go param value =
      putStrLn ("id" <> " " <> param <> " " <> value)

  
printEngineOptions :: CommandM ()
printEngineOptions =
  go "Hash" (SpinOption hashSize 1 1_048_576)
  where
    EngineOptions {..} = defaultEngineOptions
    go param option =
      putStrLn ("option name " <> param <> " " <> tshow option)


move :: UnknownMove -> CommandM ()
move mv = do
  st <- get 
  updatePosition $ makeUnknownMove mv st.position


flipPosition :: CommandM ()
flipPosition =
  modifyPosition makeNullMove


displayBoard :: CommandM ()
displayBoard = do
  putStrLn mempty
  withPosition (const print) ()
  putStrLn mempty


updatePosition :: Maybe Position -> CommandM ()
updatePosition = \case
  Just pos -> modifyPosition $ const pos
  Nothing  -> putStrLn "Error: Invalid Position"


makeUnknownMove :: UnknownMove -> Position -> Maybe Position
makeUnknownMove UnknownMove {..} pos =
  (`makeMove` pos) <$> mv
  where
  mv = find (\x -> x.start == start && x.end == end)
            (allMoves pos)


withPosition :: MonadState EngineState m => (a -> Position -> m b) -> a -> m b
withPosition f x = do
  st <-  get
  f x st.position


modifyOptions :: (EngineOptions -> EngineOptions) -> CommandM ()
modifyOptions f =
  modify \x -> x { options = f x.options }


modifyPosition :: (Position -> Position) -> CommandM ()
modifyPosition f =
  modify \x -> x { position = f x.position }
  
  

timeItPure :: MonadIO m => a -> m a
timeItPure x = timeIt do
  !result <- pure x
  pure result


type CommandM = StateT EngineState IO

