module Uci (executeCommand) where

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
  Ponderhit        -> stop
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
  EngineState{..} <- get
  let ?opts      = options
  runTask $ printSearch searchOpts position


runPerft :: Depth -> CommandM ()
runPerft depth = do
  EngineState{..} <- get
  runTask $ printPerft depth position


runDivide :: Depth -> CommandM ()
runDivide depth = do
  EngineState{..} <- get
  runTask $ printDivide depth position


printSearch :: (?opts :: EngineOptions) => SearchOptions -> Position -> IO ()
printSearch searchOpts pos =
  bracket acquire release go
  where
    acquire = do
      tTable       <- TTable.create
      killersTable <- KillersTable.create
      bestMoveRef  <- newIORef Nothing
      pure (tTable, killersTable, bestMoveRef)

    release (tTable, killersTable, bestMoveRef) = do
      liftIO $ TTable.clear tTable
      liftIO $ KillersTable.clear killersTable
      bestMove <- readIORef bestMoveRef
      traverse_ printBestMove bestMove

    go (tTable, killersTable, bestMoveRef) =
      search searchOpts bestMoveRef pos
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
printStaticEval = withPosition go
  where
    go = putStrLn
         . ("\n" <>)
         . tshow
         . getScoreBreakdown


stop :: CommandM ()
stop = do
  cancelTask
  clearTask
  where
    cancelTask = do
      task <- getTask
      traverse_ cancel task
    clearTask = do
      st <- get
      writeIORef st.task Nothing


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
  withPosition print
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


whenAvailable :: CommandM () -> CommandM ()
whenAvailable action = do
  task <- getTask
  when (isNothing task) action


runTask :: IO () -> CommandM ()
runTask action = do
  st          <- get
  let taskRef = st.task
  task        <- liftIO $ async (action *> writeIORef taskRef Nothing)
  writeIORef st.task $ Just task


getTask :: CommandM (Maybe Task)
getTask = do
  st <- get
  readIORef st.task


withPosition :: (Position -> CommandM a) -> CommandM a
withPosition f = do
  st <- get
  f st.position


modifyOptions :: (EngineOptions -> EngineOptions) -> CommandM ()
modifyOptions f =
  modify' \st -> st { options = f st.options }


modifyPosition :: (Position -> Position) -> CommandM ()
modifyPosition f =
  modify' \st -> st { position = f st.position }


type CommandM = StateT EngineState IO
