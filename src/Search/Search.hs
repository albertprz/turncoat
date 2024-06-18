module Search.Search (search, emptySearchResult, SearchResult(..)) where

import           AppPrelude               hiding ((/))
import           ClassyPrelude            ((/))

import           Models.Command
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.MoveQueries
import           MoveGen.PositionQueries
import           Search.MoveOrdering
import           Search.Perft
import           Search.Quiescence
import           Search.TimeManagement
import qualified Utils.KillersTable       as KillersTable
import           Utils.KillersTable       (KillersTable)
import qualified Utils.TranspositionTable as TTable
import           Utils.TranspositionTable (TEntry (..), TTable)

import           Control.Concurrent
import           Control.Monad.State
import           Data.Time.Clock.System


-- Features:
-- - Iterative deepening

search
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => SearchOptions -> IORef SearchResult -> Position -> IO ()
search searchOpts@SearchOptions{targetDepth, infinite} resultRef pos = do
  startTime <- getSystemTime
  maybeTimeout moveTime $ traverseUntil_ (go startTime) [1 .. targetDepth]
  when infinite $ forever $ threadDelay maxBound
  where
    go startTime depth = do
      result <- snd <$> getNodeScore initialAlpha initialBeta depth 0 pos
      endTime <- getSystemTime
      unless (null result.bestMove) $ writeIORef resultRef result
      pure (null result.bestMove || isTimeOver endTime startTime moveTime)
    moveTime = getMoveTime searchOpts pos.color


-- Features:
-- - Transposition table score caching
-- - Search extensions (Check & Single move)

negamax
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> Position -> IO (Score, Maybe Move)
negamax !alpha !beta !depth !ply pos

  | pos.halfMoveClock == 50 || isRepeatedPosition zKey pos
  = pure (0, Nothing)

  | otherwise = do
    ttResult <- liftIO $ TTable.lookupScore alpha beta extendedDepth zKey
    case ttResult of
      Just (!score, !bestMove) -> pure (score, bestMove)
      Nothing     -> cacheNodeScore alpha beta extendedDepth ply zKey pos
   where
     zKey = getZobristKey pos
     extendedDepth =
       if isKingInCheck pos || (ply < 40 && hasSingleMove)
        then depth + 1
        else depth
     hasSingleMove = hasOne $ allMoves pos
     hasOne [_] = True
     hasOne _   = False


cacheNodeScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> ZKey -> Position -> IO (Score, Maybe Move)
cacheNodeScore !alpha !beta !depth !ply !zKey pos = do
  (!score, SearchResult{bestMove}) <- getNodeScore alpha beta depth ply pos
  let
    !nodeType = getNodeType alpha beta score
    !ttScore = case nodeType of
      PV  -> score
      Cut -> beta
      All -> alpha
    !newTEntry = TEntry {
      depth = depth,
      bestMove = bestMove,
      score = ttScore,
      nodeType = nodeType,
      zobristKey = zKey
    }
  TTable.insert zKey newTEntry
  pure (score, bestMove)


-- Features:
-- - Quiescence search
-- - Null move prunning (R = 2)

getNodeScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> Position -> IO (Score, SearchResult)
getNodeScore !alpha !beta !depth !ply pos

  | depth == 0 = pure (quiesceSearch alpha beta 0 pos, emptySearchResult)

  | otherwise = do
    nullMoveScore <- getNullMoveScore beta depth ply pos
    if any (>= beta) nullMoveScore
      then pure (beta, emptySearchResult)
      else traverseMoves =<< getSortedMoves depth ply pos

  where
    traverseMoves (!moves, !hasTTMove)
      | null $ uncurry (<>) moves =
          if isKingInCheck pos
             then pure (minBound, emptySearchResult)
             else pure (       0, emptySearchResult)
      | otherwise =
        do
          let !movesSearch = getMovesScore beta depth ply moves hasTTMove pos
          (!score, (!newAlpha, !searchResult)) <-
            runStateT movesSearch (alpha, emptySearchResult)
          let !newScore = fromMaybe newAlpha score
          pure (newScore, searchResult)


getMovesScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
    ?opts :: EngineOptions)
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
     ?opts :: EngineOptions)
  => Score -> Depth -> Ply -> Bool -> Bool -> Position -> Int -> Move
  -> SearchM (Maybe Score)
getMoveScore !beta !depth !ply !isReduced !hasTTMove pos !mvIdx mv

  | isReduced && not (isCheckOrWinningCapture mv pos) =
      nullWindowSearch lmrDepth

  | hasTTMove && mvIdx > 0 =
      nullWindowSearch depth

  | otherwise =
      fullSearch

  where
    nullWindowSearch !depth' = do
     !alpha <- gets fst
     !score <- fst <$> getNegamaxScore alpha (alpha + 1) depth'
     if score > alpha
       then fullSearch
       else pure Nothing

    fullSearch = do
      !alpha             <- gets fst
      (!score, !enemyMv) <- getNegamaxScore alpha beta depth
      let !nodeType      = getNodeType alpha beta score
      !newScore          <-
        advanceState beta score ply nodeType mv enemyMv pos
      pure newScore

    !lmrFactor = min @Double 1 (fromIntegral mvIdx / 40)
    !lmrDepth  = min (depth - 1) $ ceiling
      (lmrFactor * (fromIntegral depth * 4 / 5)
        + (1 - lmrFactor) * (fromIntegral depth - 1))

    getNegamaxScore !alpha' !beta' !depth' = liftIO do
      (!score, !bestMove) <- negamax (-beta') (-alpha') (depth' - 1)
                                    (ply + 1) (makeMove mv pos)
      pure (negate score, bestMove)


getNullMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Depth -> Ply -> Position -> IO (Maybe Score)
getNullMoveScore !beta !depth !ply pos

  | depth > r && not (isKingInCheck pos)
              && not (isEndgame pos)
              && pos.materialScore >= beta =
    Just . negate . fst <$> negamax (-beta) (-alpha) (depth - r - 1)
                              (ply + 1) (makeNullMove pos)

  | otherwise = pure Nothing

  where
    !r = 2
    !alpha = beta - 1


advanceState :: (?killersTable :: KillersTable)
  => Score -> Score -> Ply -> NodeType -> Move -> Maybe Move -> Position
  -> SearchM (Maybe Score)
advanceState !beta !score !ply nodeType !mv !enemyMv pos =
  case nodeType of
    PV  -> put (score, searchResult)
            $> Nothing
    Cut -> modify' (second $ const searchResult)
            *> liftIO (KillersTable.insert ply pos mv)
            $> Just beta
    All -> pure Nothing
    where
      !searchResult = SearchResult (Just mv) enemyMv


initialAlpha :: Score
initialAlpha = minBound + 1

initialBeta :: Score
initialBeta = maxBound - 1

emptySearchResult :: SearchResult
emptySearchResult = SearchResult Nothing Nothing


data SearchResult = SearchResult {
    bestMove   :: Maybe Move
  , ponderMove :: Maybe Move
}

type SearchM = StateT (Score, SearchResult) IO
