module Search.Search (search) where

import           AppPrelude                hiding ((/))

import           Models.Command
import           Models.KillersTable       (KillersTable)
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TEntry (..), TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           MoveGen.MoveQueries
import           MoveGen.PositionQueries
import           Search.MoveOrdering
import           Search.Perft
import           Search.Quiescence
import           Search.TimeManagement

import           Control.Monad.State
import           GHC.Real                  ((/))


-- Features:
-- - Iterative deepening

search
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => SearchOptions -> IORef (Maybe Move) -> Position -> IO ()
search searchOpts bestMoveRef pos = do
  maybeTimeout moveTime
    $ void $ findLastTraverse go [1 .. searchOpts.targetDepth]
  when searchOpts.infinite (forever $ pure ())
  where
    go depth = do
      bestMove <- snd <$> getNodeScore initialAlpha initialBeta depth 0 pos
      traverse (writeIORef bestMoveRef . Just) bestMove
    moveTime = getMoveTime searchOpts pos.color


-- Features:
-- - Transposition table score caching
-- - Search extensions (Check & Single move)

negamax
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> Position -> IO Score
negamax !alpha !beta !depth !ply pos

  | pos.halfMoveClock == 50 || isRepeatedPosition zKey pos = pure 0

  | otherwise = do
    ttScore <- liftIO $ TTable.lookupScore alpha beta extendedDepth zKey
    case ttScore of
      Just !score -> pure score
      Nothing     -> cacheNodeScore alpha beta extendedDepth ply zKey pos
   where
     !zKey = getZobristKey pos
     !extendedDepth =
       if isKingInCheck pos || (ply < 40 && hasSingleMove)
        then depth + 1
        else depth
     hasSingleMove = hasOne $ allMoves pos
     hasOne [_] = True
     hasOne _   = False


cacheNodeScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> ZKey -> Position -> IO Score
cacheNodeScore !alpha !beta !depth !ply !zKey pos = do
  (!score, !bestMove) <- getNodeScore alpha beta depth ply pos
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
  pure score


-- Features:
-- - Quiescence search
-- - Null move prunning (R = 2)

getNodeScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Score -> Depth -> Ply -> Position -> IO (Score, Maybe Move)
getNodeScore !alpha !beta !depth !ply pos

  | depth == 0 = pure (quiesceSearch alpha beta 0 pos, Nothing)

  | otherwise = do
    nullMoveScore <- getNullMoveScore beta depth ply pos
    if any (>= beta) nullMoveScore
      then pure (beta, Nothing)
      else traverseMoves =<< getSortedMoves depth ply pos

  where
    traverseMoves (!moves, !hasTTMove)
      | null $ uncurry (<>) moves =
          if isKingInCheck pos
             then pure (minBound, Nothing)
             else pure (       0, Nothing)
      | otherwise =
        do
          let !movesScore = getMovesScore beta depth ply moves hasTTMove pos
          (!score, (!newAlpha, !bestMove)) <-
            runStateT movesScore (alpha, Nothing)
          let !newScore = fromMaybe newAlpha score
          pure (newScore, bestMove)


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
  => Score -> Depth -> Ply -> Bool -> Bool -> Position -> Int -> Move -> SearchM (Maybe Score)
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
     !score <- getNegamaxScore alpha (alpha + 1) depth'
     if score > alpha
       then fullSearch
       else pure Nothing

    fullSearch = do
      !alpha        <- gets fst
      !score        <- getNegamaxScore alpha beta depth
      let !nodeType = getNodeType alpha beta score
      !newScore     <- advanceState beta score ply nodeType mv pos
      pure newScore

    !lmrFactor = min @Double 1 (fromIntegral mvIdx / 40)
    !lmrDepth  = min (depth - 1) $ ceiling
      (lmrFactor * (fromIntegral depth * 4 / 5)
        + (1 - lmrFactor) * (fromIntegral depth - 1))

    getNegamaxScore !alpha' !beta' !depth' = liftIO do
      !score <- negamax (-beta') (-alpha') (depth' - 1)
                                 (ply + 1) (makeMove mv pos)
      pure $! negate score



getNullMoveScore
  :: (?killersTable :: KillersTable, ?tTable :: TTable,
     ?opts :: EngineOptions)
  => Score -> Depth -> Ply -> Position -> IO (Maybe Score)
getNullMoveScore !beta !depth !ply pos

  | depth > r && not (isKingInCheck pos)
              && not (isEndgame pos)
              && pos.materialScore >= beta =
    Just . negate <$> negamax (-beta) (-alpha) (depth - r - 1)
                              (ply + 1) (makeNullMove pos)

  | otherwise = pure Nothing

  where
    !r = 2
    !alpha = beta - 1


advanceState :: (?killersTable :: KillersTable)
  => Score -> Score -> Ply -> NodeType -> Move -> Position -> SearchM (Maybe Score)
advanceState !beta !score !ply nodeType !mv pos =
  case nodeType of
    PV  -> put (score, Just mv)
            $> Nothing
    Cut -> modify' (second $ const $ Just mv)
            *> liftIO (KillersTable.insert ply pos mv)
            $> Just beta
    All -> pure Nothing


initialAlpha :: Score
initialAlpha = minBound + 1

initialBeta :: Score
initialBeta = maxBound - 1


type SearchM = StateT (Score, Maybe Move) IO
