module Search.Search (search, emptySearchResult, SearchResult(..)) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Command
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.MoveQueries
import           MoveGen.PositionQueries
import           Search.MoveOrdering
import           Search.Parameters
import           Search.Perft
import           Search.Quiescence
import           Search.TimeManagement
import qualified Utils.KillersTable       as KillersTable
import           Utils.KillersTable       (KillersTable)
import qualified Utils.TranspositionTable as TEntry (TEntry (..))
import qualified Utils.TranspositionTable as TTable
import           Utils.TranspositionTable (TEntry (TEntry), TTable)

import           Control.Concurrent
import           Control.Monad.State
import           Data.Time.Clock.System


-- Features:
-- - Iterative deepening

search
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?age :: Age)
  => SearchOptions -> IORef SearchResult -> Position -> IO ()
search searchOpts@SearchOptions{..} resultRef pos = do
  startTime <- getSystemTime
  nodesRef <- newIORef 0
  let ?nodes = nodesRef
  maybeTimeout timeToMove $ untilM (go startTime nodesRef) [1 .. targetDepth]
  when infinite $ forever $ threadDelay maxBound
  where
    timeToMove =
      maybeFilter (const $ not infinite) $ getMoveTime searchOpts pos

    go startTime nodesRef depth = do
      result  <- getNodeResult initialAlpha initialBeta depth 0 pos
      endTime <- getSystemTime
      nodes   <- readIORef nodesRef
      printSearchInfo depth nodes (endTime |-| startTime) result
      let bestMove = result.bestMove <|> headMay (allMoves pos)
      unless (null bestMove)
         $ writeIORef resultRef result {bestMove = bestMove}
      pure (isTimeOver endTime startTime timeToMove
            || hasSingleMove pos
            || any (nodes >=) maxNodes
            || getGameResult result.score `elem` [Victory, Defeat])


-- Features:
-- - Transposition table score caching
-- - Search extensions (Check & Single move)

negamax
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Score -> Depth -> Ply -> Position -> IO SearchResult
negamax !alpha !beta !depth !ply pos

  | pos.halfMoveClock == 50 || isRepeatedPosition zKey pos
  = pure $! emptySearchResult 0

  | otherwise = do
    ttResult <- liftIO $ TTable.lookupScore alpha beta extendedDepth zKey
    case ttResult of
      Just (!score, !bestMove) -> pure $! SearchResult score bestMove Nothing
      Nothing -> cacheNodeResult alpha beta extendedDepth ply zKey pos
   where
     zKey = getZobristKey pos
     extendedDepth =
       if isKingInCheck pos || (ply < 40 && hasSingleMove pos)
        then depth + 1
        else depth


cacheNodeResult
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Score -> Depth -> Ply -> ZKey -> Position -> IO SearchResult
cacheNodeResult !alpha !beta !depth !ply !zKey pos = do
  modifyIORef' ?nodes (+ 1)
  searchResult@SearchResult {..} <- getNodeResult alpha beta depth ply pos
  let
    !nodeType = getNodeType alpha beta score
    !ttScore = case nodeType of
      PV  -> score
      Cut -> beta
      All -> alpha
    !newTEntry = TEntry {
        TEntry.depth      = depth
      , TEntry.bestMove   = bestMove
      , TEntry.score      = ttScore
      , TEntry.nodeType   = nodeType
      , TEntry.zobristKey = zKey
      , TEntry.age        = ?age
    }
  TTable.insert zKey newTEntry
  pure searchResult


-- Features:
-- - Quiescence search
-- - Null move prunning
-- - Futility prunning

getNodeResult
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Score -> Depth -> Ply -> Position -> IO SearchResult
getNodeResult !alpha !beta !depth !ply pos

  | depth == 0
  || depth <= 3
    && not (isKingInCheck pos)
    && staticEval < 1000 && staticEval > -1000
    && staticEval + futilityMargin <= alpha
  = emptySearchResult <$> quiesceSearch alpha beta 0 pos

  | otherwise = do
    nullMoveScore <- getNullMoveScore beta depth ply pos
    if any (>= beta) nullMoveScore
      then pure $! emptySearchResult beta
      else traverseMoves =<< getSortedMoves depth ply pos

  where
    staticEval      = evaluatePosition pos
    !futilityMargin = futilityMargins !! (fromIntegral depth - 1)
    traverseMoves moves
      | null $ uncurry (<>) moves =
          let score | isKingInCheck pos = minScore
                    | otherwise         = 0
          in pure $! emptySearchResult score
      | otherwise = do
          let movesSearch = getMovesScore beta depth ply moves pos
          (!score, !searchResult) <-
            runStateT movesSearch (emptySearchResult alpha)
          let
            !newAlpha = searchResult.score
            !newScore = fromMaybe newAlpha score
          pure $! searchResult {score = newScore}


getMovesScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
    ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> ([Move], [Move]) -> Position
  -> SearchM (Maybe Score)
getMovesScore !beta !depth !ply (mainMoves, reducedMoves) pos = do
  mainSearchScore <- mainMovesSearch
  maybe reducedMovesSearch (pure . Just) mainSearchScore

  where
    mainMovesSearch       = movesSearch False mainMoves
    reducedMovesSearch    = movesSearch True  reducedMoves
    movesSearch isReduced =
      findTraverseIndex (getMoveScore beta depth ply isReduced pos)


-- Features:
-- - Principal Variation Search
-- - Late Move Reductions

getMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> Bool -> Position -> Int -> Move -> SearchM (Maybe Score)
getMoveScore !beta !depth !ply !isReduced pos !mvIdx !mv = do
  if isReduced || mvIdx > 0
    then nullWindowSearch
    else fullSearch
  where
    !lmrDepth
      | isReduced && not (isCheckMove mv pos || isCapture mv pos)
        = getLmrDepth mvIdx depth
      | otherwise
        = depth

    nullWindowSearch = do
     st@SearchResult {score = alpha} <- get
     SearchResult {score} <- getScore alpha (alpha + 1)
     put st
     if score > alpha
       then fullSearch
       else pure Nothing

    fullSearch = do
      SearchResult {score = alpha} <- get
      SearchResult{..} <- getScore alpha beta
      let nodeType    = getNodeType alpha beta score
      newScore <- advanceState beta score ply nodeType mv bestMove pos
      pure $! newScore

    getScore !alpha' !beta' = liftIO
      $ getNegamaxScore alpha' beta' lmrDepth ply (makeMove mv pos)


getNullMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> Position -> IO (Maybe Score)
getNullMoveScore !beta !depth !ply pos

  | depth > r && not (isKingInCheck pos)
              && not (isEndgame pos) = do
    SearchResult {..} <- getNegamaxScore alpha beta reducedDepth ply
                                       (makeNullMove pos)
    pure $ Just score

  | otherwise = pure Nothing

  where
    reducedDepth = depth - r
    alpha        = beta - 1
    r            = 2


getNegamaxScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Score -> Depth -> Ply -> Position -> IO SearchResult
getNegamaxScore !alpha !beta !depth !ply !pos = do
  result <- negamax (-beta) (-alpha) (depth - 1) (ply + 1) pos
  pure $ result {score = - result.score}


advanceState :: (?killersTable :: KillersTable)
  => Score -> Score -> Ply -> NodeType -> Move -> Maybe Move -> Position
  -> SearchM (Maybe Score)
advanceState !beta !score !ply !nodeType !mv !enemyMv pos =
  case nodeType of
    PV  -> put searchResult
            $> Nothing
    Cut -> put searchResult
            *> liftIO (KillersTable.insert ply pos mv)
            $> Just beta
    All -> pure Nothing
    where
      !searchResult = SearchResult score (Just mv) enemyMv


getGameResult :: Score -> GameResult
getGameResult score
  | score == maxScore = Victory
  | score == minScore = Defeat
  | otherwise        = InProgress


printSearchInfo :: Depth -> Word64 -> MicroSeconds -> SearchResult -> IO ()
printSearchInfo depth nodes elapsedTime SearchResult{..} = do
  putStrLn ("info"
    <> " depth " <> tshow depth
    <> " score " <> showScore
    <> " nodes " <> tshow nodes
    <> " nps "   <> tshow nps
    <> " time "  <> tshow timeMillis
    <> " pv "    <> unwords (tshow <$> pv))
  hFlush stdout
  where
    nps        = nodes * 1_000_000 `div` elapsedTime
    timeMillis = elapsedTime `div` 1_000
    pv         = catMaybes [bestMove, ponderMove]
    showScore = case getGameResult score of
      InProgress -> "cp "   <> tshow score
      Victory    -> "mate " <> tshow @Int 1
      Defeat     -> "mate " <> tshow @Int (-1)


emptySearchResult :: Score -> SearchResult
emptySearchResult score = SearchResult score Nothing Nothing


data SearchResult = SearchResult {
    score      :: Score
  , bestMove   :: Maybe Move
  , ponderMove :: Maybe Move
}


data GameResult
  = InProgress
  | Victory
  | Defeat
  deriving (Eq)


type SearchM = StateT SearchResult IO
