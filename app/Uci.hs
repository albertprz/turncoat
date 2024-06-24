module Uci (executeCommand, initialEngineState, putStrLnFlush) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Command
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           Search.Perft
import           Search.Search
import           Search.TimeManagement
import qualified Utils.KillersTable              as KillersTable
import           Utils.KillersTable              (KillersTable)
import qualified Utils.TranspositionTable        as TTable
import           Utils.TranspositionTable        (TTable)

import           Control.Concurrent.Thread.Delay
import           Control.Monad.State
import           Data.Composition
import qualified Data.Map                        as Map
import           Data.Time.Clock.System
import           System.Exit


executeCommand :: Command -> CommandM ()
executeCommand = \case
  Uci              -> handleStart
  UciNewGame       -> handleNewGame
  IsReady          -> printReady
  Search opts      -> whenAvailable $ runSearch opts
  Perft  depth     -> whenAvailable $ runPerft  depth
  Divide depth     -> whenAvailable $ runDivide depth
  Ponderhit        -> handlePonderhit
  Stop             -> stop
  Quit             -> quit
  SetPosition pos  -> setPosition pos
  SetOption option -> setOption   option
  MakeMoves mvs    -> makeMoves   mvs
  Evaluate         -> printStaticEval
  Display          -> displayBoard
  Flip             -> flipPosition


runSearch :: SearchOptions -> CommandM ()
runSearch searchOpts = do
  EngineState{..} <- get
  killersTable   <- liftIO KillersTable.create
  let ?opts         = options
      ?tTable       = tTable
      ?killersTable = killersTable
      ?age          = age
  runTask $ printSearch searchOpts position
  startTime <- liftIO getSystemTime
  modify' \st -> st { searchStart = startTime, searchOptions = searchOpts }
  incrementAge


runPerft :: Depth -> CommandM ()
runPerft depth = do
  EngineState{..} <- get
  runTask $ printPerft depth position


runDivide :: Depth -> CommandM ()
runDivide depth = do
  EngineState{..} <- get
  runTask $ printDivide depth position


printSearch
  :: (?opts :: EngineOptions, ?tTable :: TTable,
     ?killersTable :: KillersTable, ?age :: Age)
  => SearchOptions -> Position -> IO ()
printSearch searchOpts pos = do
  bracket acquire release go
  where
    acquire =
      newIORef $ emptySearchResult 0

    release searchResultRef =
      printBestMove =<< readIORef searchResultRef

    go searchResultRef =
      search searchOpts searchResultRef pos


printPerft :: Depth -> Position -> IO ()
printPerft = putStrLnFlush . tshow .: perft


printDivide :: Depth -> Position -> IO ()
printDivide =
  void
  . Map.traverseWithKey (\k v -> putStrLnFlush (tshow k <> ": " <> tshow v))
  .: divide


printStaticEval :: CommandM ()
printStaticEval = withPosition go
  where
    go = putStrLnFlush
         . ("\n" <>)
         . tshow
         . getScoreBreakdown


handlePonderhit :: CommandM ()
handlePonderhit = do
  EngineState {..} <- get
  now <- liftIO getSystemTime
  let elapsedTime = now |-| searchStart
      moveTime    = getMoveTime searchOptions position.color
  if isTimeOver now searchStart moveTime
    then stop
    else liftIO do
      let timeOver = getTimeOver $ fromMaybe 0 moveTime
      void $ async do
        delay $ fromIntegral (timeOver - elapsedTime)
        stopTask taskRef


stop :: CommandM ()
stop = do
  st <- get
  liftIO $ stopTask st.taskRef


quit :: CommandM ()
quit = do
  stop
  liftIO exitSuccess


handleNewGame :: CommandM ()
handleNewGame = do
  EngineState {tTable} <- get
  liftIO $ TTable.reset tTable
  updatePosition $ Just startPosition
  modify' \st -> st {age = 0}


handleStart :: CommandM ()
handleStart = do
  printEngineInfo
  putStrLnFlush mempty
  printEngineOptions
  printUciReady


setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  updatePosition
    $ foldM (flip makeUnknownMove) initialPosition moves


setOption :: OptionSpec -> CommandM ()
setOption = \case
  Ponder ponder ->
    modifyOptions \x -> x { ponder = ponder }

  HashSize size -> do
    modifyOptions \x -> x { hashSize = fromIntegral size }
    EngineState {options} <- get
    newTTable <- liftIO $ TTable.create options
    modify' \st -> st { tTable = newTTable }

  ClearHash -> do
    EngineState {tTable} <- get
    liftIO $ TTable.reset tTable


printBestMove :: MonadIO m => SearchResult -> m ()
printBestMove SearchResult {..}
  | Just mv  <- bestMove
  , Just mv2 <- ponderMove
    = putStrLnFlush ("bestmove " <> tshow mv <> " ponder " <> tshow mv2)
  | Just mv  <- bestMove
    = putStrLnFlush ("bestmove " <> tshow mv)
  | otherwise
    = putStrLnFlush mempty


printUciReady :: CommandM ()
printUciReady = putStrLnFlush "uciok"


printReady :: CommandM ()
printReady = putStrLnFlush "readyok"


printEngineInfo :: CommandM ()
printEngineInfo = do
  go "name"  (name <> " " <> version)
  go "author" author
  where
    EngineInfo {..} = engineInfo
    go param value =
      putStrLnFlush ("id" <> " " <> param <> " " <> value)


printEngineOptions :: CommandM ()
printEngineOptions = do
  go "Hash"       (SpinOption hashSize 1 maxBound)
  go "Clear Hash" ButtonOption
  go "Ponder"     (CheckOption ponder)
  where
    EngineOptions {..} = defaultEngineOptions
    go param option =
      putStrLnFlush ("option name " <> param <> " " <> tshow option)


makeMoves :: [UnknownMove] -> CommandM ()
makeMoves mvs = do
  st <- get
  setPosition $ PositionSpec st.position mvs


flipPosition :: CommandM ()
flipPosition =
  modifyPosition makeNullMove


displayBoard :: CommandM ()
displayBoard = do
  putStrLnFlush mempty
  withPosition print
  putStrLnFlush mempty


updatePosition :: Maybe Position -> CommandM ()
updatePosition = \case
  Just pos -> modifyPosition $ const pos
  Nothing  -> putStrLnFlush "Error: Invalid Position"


makeUnknownMove :: UnknownMove -> Position -> Maybe Position
makeUnknownMove UnknownMove {..} pos =
  (`makeMove` pos) <$> find isSameMove (allMoves pos)
  where
    isSameMove mv =
      mv.start == start && mv.end == end && mv.promotion == promotion


whenAvailable :: CommandM () -> CommandM ()
whenAvailable action = do
  task <- getTask
  when (isNothing task) action


runTask :: IO () -> CommandM ()
runTask action = do
  st <- get
  task        <- liftIO $ async (action *> writeIORef st.taskRef Nothing)
  writeIORef st.taskRef $ Just task


stopTask :: IORef (Maybe Task) -> IO ()
stopTask taskRef = do
  task <- readIORef taskRef
  traverse_ cancel task
  writeIORef taskRef Nothing


getTask :: CommandM (Maybe Task)
getTask = do
  st <- get
  readIORef st.taskRef


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


incrementAge :: CommandM ()
incrementAge =
  modify' \st -> st {age = if st.age == 127 then 0 else st.age + 1}


data EngineState = EngineState
  { position      :: Position
  , options       :: EngineOptions
  , taskRef       :: IORef (Maybe Task)
  , searchStart   :: SystemTime
  , searchOptions :: SearchOptions
  , tTable        :: TTable
  , age           :: Word8
  }


initialEngineState :: IO EngineState
initialEngineState = do
  taskRef <- newIORef Nothing
  tTable  <- TTable.create defaultEngineOptions
  pure EngineState
    { position      = startPosition
    , options       = defaultEngineOptions
    , taskRef       = taskRef
    , searchStart   = MkSystemTime 0 0
    , searchOptions = defaultSearchOptions
    , tTable        = tTable
    , age           = 0
    }


type CommandM = StateT EngineState IO


putStrLnFlush :: MonadIO m => Text -> m ()
putStrLnFlush message = do
  putStrLn message
  hFlush stdout
