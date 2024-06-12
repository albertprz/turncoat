module Uci (executeCommand) where

import           AppPrelude

import           Evaluation.Evaluation
import qualified Models.KillersTable       as KillersTable
import           Models.Command
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
import           System.Exit


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Uci              -> handleStart
  UciNewGame       -> handleNewGame
  IsReady          -> printReady
  Search opts      -> whenAvailable $ runSearch opts
  Perft  depth     -> whenAvailable $ runPerft  depth
  Divide depth     -> whenAvailable $ runDivide depth
  Stop             -> stop
  Quit             -> quit
  SetPosition pos  -> setPosition pos
  SetOption option -> setOption   option
  MakeMove mv      -> move        mv
  Evaluate         -> printStaticEval
  Display          -> displayBoard
  Flip             -> flipPosition


runSearch :: SearchOptions -> CommandM ()
runSearch searchOpts = do
  st   <- get
  let ?options = st.options
  runTask $ printSearch st.bestMove searchOpts st.position


runPerft :: Depth -> CommandM ()
runPerft depth = do
  st <- get
  runTask $ printPerft depth st.position


runDivide :: Depth -> CommandM ()
runDivide depth = do
  st <- get
  runTask $ printDivide depth st.position


printSearch :: (?options::EngineOptions)
  => IORef (Maybe Move) -> SearchOptions -> Position -> IO ()
printSearch bestMoveRef searchOpts pos =
  bracket acquire release go
  where
    acquire = do
      tTable       <- TTable.create
      killersTable <- KillersTable.create
      pure (tTable, killersTable)
    release (tTable, killersTable) = do
      liftIO $ TTable.clear       tTable
      liftIO $ KillersTable.clear killersTable
    go (tTable, killersTable) = do
      traverse_ printBestMove
        =<< getBestMove bestMoveRef searchOpts.depth pos
      writeIORef bestMoveRef Nothing
      where
        ?tTable       = tTable
        ?killersTable = killersTable


printPerft :: Depth -> Position -> IO ()
printPerft = putStrLn . tshow .: perft


printDivide :: Depth -> Position -> IO ()
printDivide =
  void
  . Map.traverseWithKey (\k v -> putStrLn (tshow k <> ": " <> tshow v))
  .: divide


printStaticEval :: CommandM ()
printStaticEval = withPosition (const go) ()
  where
    go = putStrLn
         . ("\n" <>)
         . tshow
         . getScoreBreakdown


stop :: CommandM ()
stop = do
  task <- getTask
  traverse_ cancel task
  traverse_ printBestMove =<< getLastBestMove
  clearBestMove
  clearTask


quit :: CommandM ()
quit = do
  stop
  liftIO exitSuccess


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


printBestMove :: MonadIO m => Move -> m ()
printBestMove = putStrLn . ("bestmove " ++) . tshow


printUciReady :: CommandM ()
printUciReady = putStrLn "uciok"


printReady :: CommandM ()
printReady = putStrLn "readyok"


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
  modify \st -> st { options = f st.options }


modifyPosition :: (Position -> Position) -> CommandM ()
modifyPosition f =
  modify \st -> st { position = f st.position }


whenAvailable :: CommandM () -> CommandM ()
whenAvailable action = do
  task <- getTask
  when (isNothing task) action


runTask :: IO () -> CommandM ()
runTask action = do
  st <- get
  let taskRef = st.task
  task <- liftIO $ async (action *> writeIORef taskRef Nothing)
  putTask task


putTask :: Task -> CommandM ()
putTask task = do
  st <- get
  writeIORef st.task $ Just task


clearTask :: CommandM ()
clearTask = do
  st <- get
  writeIORef st.task Nothing


getTask :: CommandM (Maybe Task)
getTask = do
  st <- get
  readIORef st.task


getLastBestMove :: CommandM (Maybe Move)
getLastBestMove = do
  st <- get
  readIORef st.bestMove


clearBestMove :: CommandM ()
clearBestMove = do
  st <- get
  writeIORef st.bestMove Nothing


type CommandM = StateT EngineState IO
