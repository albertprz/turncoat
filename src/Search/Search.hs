module Search.Search (getBestMove) where

import           AppPrelude

import           Models.KillersTable       (KillersTable)
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TEntry (..), TTable, ZKey)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           Search.MoveOrdering
import           Search.Quiescence

import           Control.Monad.State
import           Evaluation.Material
import           MoveGen.MoveQueries


-- Features:
-- - Iterative deepening

{-# INLINE  getBestMove #-}
getBestMove :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Depth -> Position -> IO (Maybe Move)
getBestMove !depth pos =
  lastEx <$> evalStateT (traverse (`aspirationSearch` pos) [1 .. depth])
                        (initialAlpha, initialBeta)


-- Features:
-- - Aspiration windows

{-# INLINE  aspirationSearch #-}
aspirationSearch :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Depth -> Position -> AspirationSearchM (Maybe Move)
aspirationSearch !depth pos  = do
  (!alpha, !beta) <- get
  (!score, !mv) <- liftIO $ getNodeScore alpha beta depth 0 pos
  case getNodeType alpha beta score of
    PV  -> put (score - windowDelta, score + windowDelta) $> mv
    Cut -> modify (second (+ windowDelta))       *> aspirationSearch depth pos
    All -> modify (first (\x -> x - windowDelta)) *> aspirationSearch depth pos


-- Features:
-- - Transposition table score caching

{-# INLINE  negamax #-}
negamax :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Score -> Depth -> Ply -> Position -> IO Score
negamax !alpha !beta !depth !ply pos
  | pos.halfMoveClock == 50 || isRepeatedPosition zKey pos = pure 0
  | otherwise = do
    ttScore <- liftIO $ TTable.lookupScore alpha beta depth zKey
    case ttScore of
      Just !score -> pure score
      Nothing     -> cacheNodeScore alpha beta depth ply zKey pos
   where
     !zKey = getZobristKey pos


{-# INLINE  cacheNodeScore #-}
cacheNodeScore :: (?killersTable :: KillersTable, ?tTable:: TTable)
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
-- - Futility prunning
-- - Null move prunning (R = 2)

{-# INLINE  getNodeScore #-}
getNodeScore :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Score -> Depth -> Ply -> Position -> IO (Score, Maybe Move)
getNodeScore !alpha !beta !depth !ply pos

  | depth == 0 = pure (quiesceSearch alpha beta 0 pos, Nothing)

  | depth == 1 && not (isKingInCheck pos)
              && pos.materialScore <= threshold = do
      tacticalMoves <- getSortedFutilityMoves threshold pos
      if null tacticalMoves
       then pure (pos.materialScore, Nothing)
       else traverseMoves (tacticalMoves, [], [], [])

  | otherwise = do
    nullMoveScore <- getNullMoveScore beta depth ply pos
    if any (>= beta) nullMoveScore
      then pure (beta, Nothing)
      else traverseMoves =<< getSortedMoves depth ply pos

  where
    threshold = alpha - 2 * pawnScore
    traverseMoves moves
      | null $ tuple4Append moves =
          if isKingInCheck pos
             then pure (minBound + 1, Nothing)
             else pure (0, Nothing)
      | otherwise =
        do (!score, (!newAlpha, !bestMove)) <-
             runStateT (getMovesScore beta depth ply pos moves)
                       (alpha, Nothing)
           pure (fromMaybe newAlpha score, bestMove)


-- Features:
-- - Late Move Reductions

{-# INLINE  getMovesScore #-}
getMovesScore :: (?killersTable :: KillersTable, ?tTable::TTable)
  => Score -> Depth -> Ply -> Position -> ([Move], [Move], [Move], [Move])
  -> SearchM (Maybe Score)
getMovesScore !beta !depth !ply pos
  (mainMoves, reducedMoves, reduced2Moves, reduced3Moves) = do
  mainSearchScore <- mainMovesSearch
  foldM accumulateScore mainSearchScore
        [reducedMovesSearch, reduced2MovesSearch, reduced3MovesSearch]
  where
    mainMovesSearch      = movesSearch depth       mainMoves
    reducedMovesSearch   = movesSearch (depth - 1) reducedMoves
    reduced2MovesSearch  = movesSearch (depth - 2) reduced2Moves
    reduced3MovesSearch  = movesSearch (depth - 3) reduced3Moves

    accumulateScore score searchFn =
      maybe searchFn (pure . Just) score
    movesSearch lmrDepth =
      findTraverse (getMoveScore beta depth lmrDepth ply pos)


{-# INLINE  getMoveScore #-}
getMoveScore :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Depth -> Depth -> Ply -> Position -> Move -> SearchM (Maybe Score)
getMoveScore !beta !depth !lmrDepth !ply pos move

  | lmrDepth < depth && not (isCheckOrCapture move pos) = do
     !alpha <- gets fst
     let !betaWindow = alpha + 1
     !score   <- negate <$> liftIO (negamax (-betaWindow) (-alpha)
                                           (lmrDepth - 1)
                                           (ply + 1) (makeMove move pos))
     if score > alpha
       then getMoveScore beta depth (lmrDepth + 1) ply pos move
       else pure Nothing

  | otherwise = do
     !alpha <- gets fst
     !score   <- negate <$> liftIO (negamax (-beta) (-alpha) (depth - 1)
                                           (ply + 1) (makeMove move pos))
     let !nodeType = getNodeType alpha beta score
     advanceState beta score ply nodeType move pos


{-# INLINE  getNullMoveScore #-}
getNullMoveScore :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Depth -> Ply -> Position -> IO (Maybe Score)
getNullMoveScore !beta !depth !ply pos

  | depth >= 3 && not (isKingInCheck pos) && pos.materialScore >= beta =
    Just . negate <$> negamax (-beta) (-alpha) (depth - r - 1)
                              (ply + 1) (makeNullMove pos)

  | otherwise = pure Nothing

  where
    !r = 2
    !alpha = beta - 1


{-# INLINE  advanceState #-}
advanceState :: (?killersTable :: KillersTable)
  => Score -> Score -> Ply -> NodeType -> Move -> Position -> SearchM (Maybe Score)
advanceState !beta !score !ply !nodeType !move pos =
  case nodeType of
    PV  -> put (score, Just move)
            $> Nothing
    Cut -> modify' (second $ const $ Just move)
            *> liftIO (KillersTable.insert ply pos move)
            $> Just beta
    All -> pure Nothing


initialAlpha :: Score
initialAlpha = minBound + 1

initialBeta :: Score
initialBeta = maxBound - 1

windowDelta :: Score
windowDelta = 10


type SearchM           = StateT (Score, Maybe Move) IO
type AspirationSearchM = StateT (Score, Score) IO
