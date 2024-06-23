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
import           Models.Piece


-- Features:
-- - Iterative deepening

search
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?age :: Age)
  => SearchOptions -> IORef SearchResult -> Position -> IO ()
search searchOpts@SearchOptions{targetDepth, infinite} resultRef pos = do
  startTime <- getSystemTime
  nodesRef <- newIORef 0
  let ?nodes = nodesRef
  maybeTimeout moveTime
    $ untilM (go startTime nodesRef) [1 .. targetDepth]
  when infinite $ forever $ threadDelay maxBound
  where
    moveTime = getMoveTime searchOpts pos.color
    go startTime nodesRef depth = do
      result <- getNodeResult initialAlpha initialBeta depth 0 pos
      endTime <- getSystemTime
      nodes <- readIORef nodesRef
      printSearchInfo pos.color depth nodes (endTime |-| startTime) result
      unless (null result.bestMove) $ writeIORef resultRef result
      pure (null result.bestMove || isTimeOver endTime startTime moveTime)


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
       if isKingInCheck pos || (ply < 40 && hasSingleMove)
        then depth + 1
        else depth
     hasSingleMove = hasOne $ allMoves pos
     hasOne [_] = True
     hasOne _   = False


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
  || depth <= 3 && not (isKingInCheck pos)
              && evaluatePosition pos + futilityMargin <= alpha
  = emptySearchResult <$> quiesceSearch alpha beta 0 pos

  | otherwise = do
    nullMoveScore <- getNullMoveScore beta depth ply pos
    if any (>= beta) nullMoveScore
      then pure $! emptySearchResult beta
      else traverseMoves =<< getSortedMoves depth ply pos

  where
    !futilityMargin = futilityMargins !! (fromIntegral depth - 1)
    traverseMoves (moves, hasTTMove)
      | null $ uncurry (<>) moves =
          let score | isKingInCheck pos = minScore
                    | otherwise         = 0
          in pure $! emptySearchResult score
      | otherwise = do
          let movesSearch = getMovesScore beta depth ply moves hasTTMove pos
          (!score, !searchResult) <-
            runStateT movesSearch (emptySearchResult alpha)
          let
            !newAlpha = searchResult.score
            !newScore = fromMaybe newAlpha score
          pure $! searchResult {score = newScore}


getMovesScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
    ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> ([Move], [Move]) -> Bool -> Position
  -> SearchM (Maybe Score)
getMovesScore !beta !depth !ply (mainMoves, reducedMoves) hasTTMove pos = do
  mainSearchScore <- mainMovesSearch
  maybe reducedMovesSearch (pure . Just) mainSearchScore

  where
    mainMovesSearch       = movesSearch False mainMoves
    reducedMovesSearch    = movesSearch True  reducedMoves
    movesSearch isReduced =
      findTraverseIndex (getMoveScore beta depth ply isReduced hasTTMove pos)


-- Features:
-- - Late Move Reductions
-- - Principal Variation Search

getMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> Bool -> Bool -> Position -> Int -> Move
  -> SearchM (Maybe Score)
getMoveScore !beta !depth !ply !isReduced !hasTTMove pos !mvIdx mv

  | isReduced && not (isCheckMove mv pos) =
      nullWindowSearch $! getLmrDepth mvIdx depth

  | hasTTMove && mvIdx > 0 =
      nullWindowSearch depth

  | otherwise =
      fullSearch

  where
    nullWindowSearch !depth' = do
     SearchResult {score = alpha} <- get
     !score <- fst <$> getNegamaxScore alpha (alpha + 1) depth'
     if score > alpha
       then fullSearch
       else pure Nothing

    fullSearch = do
      SearchResult {score = alpha} <- get
      (!score, !enemyMv)           <- getNegamaxScore alpha beta depth
      let !nodeType                = getNodeType alpha beta score
      !newScore                    <-
        advanceState beta score ply nodeType mv enemyMv pos
      pure newScore

    getNegamaxScore !alpha' !beta' !depth' = liftIO do
      SearchResult {..} <- negamax (-beta') (-alpha') (depth' - 1)
                                 (ply + 1) (makeMove mv pos)
      pure (negate $! score, bestMove)


getNullMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions, ?nodes :: IORef Word64, ?age :: Age)
  => Score -> Depth -> Ply -> Position -> IO (Maybe Score)
getNullMoveScore !beta !depth !ply pos

  | depth > r && not (isKingInCheck pos)
              && not (isEndgame pos)
              && pos.materialScore >= beta = do
    SearchResult {..} <- negamax (-beta) (-alpha) (depth - r - 1)
                               (ply + 1) (makeNullMove pos)
    pure $! Just $! negate score

  | otherwise = pure Nothing

  where
    r = 2
    alpha = beta - 1


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


printSearchInfo
  :: Color -> Depth -> Word64 -> MicroSeconds -> SearchResult -> IO ()
printSearchInfo color depth nodes timePassed SearchResult{..} = do
  putStrLn (
         "info depth "    <> tshow depth
      <> " score cp "     <> tshow colorScore
      <> " nodes "        <> tshow nodes
      <> " nps "          <> tshow nps
      <> " time "         <> tshow timeMillis
      <> " pv "           <> unwords (tshow <$> pv))
  hFlush stdout
  where
    colorScore
      | White <- color =   score
      | Black <- color = - score
    nps        = nodes * 1_000_000 `div` timePassed
    timeMillis = timePassed `div` 1_000
    pv         = catMaybes [bestMove, ponderMove]


emptySearchResult :: Score -> SearchResult
emptySearchResult score = SearchResult score Nothing Nothing


data SearchResult = SearchResult {
    score      :: Score
  , bestMove   :: Maybe Move
  , ponderMove :: Maybe Move
}


type SearchM = StateT SearchResult IO
