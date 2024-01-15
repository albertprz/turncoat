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



{-# INLINE  getBestMove #-}
getBestMove :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Depth -> Position -> IO (Maybe Move)
getBestMove !depth pos =
  lastEx <$> evalStateT (traverse (`aspirationSearch` pos) [0 .. depth])
                        (initialAlpha, initialBeta)


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


{-# INLINE  negamax #-}
negamax :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Score -> Depth -> Ply -> Position -> IO Score
negamax !alpha !beta !depth !ply pos = do
    let !zKey = getZobristKey pos
    !ttScore <- liftIO $ TTable.lookupScore alpha beta depth zKey
    case ttScore of
      Just !score -> pure score
      Nothing     -> cacheNodeScore alpha beta depth ply pos zKey


{-# INLINE  cacheNodeScore #-}
cacheNodeScore :: (?killersTable :: KillersTable, ?tTable:: TTable)
  => Score -> Score -> Depth -> Ply -> Position -> ZKey -> IO Score
cacheNodeScore !alpha !beta !depth !ply pos !zKey = do
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


{-# INLINE  getNodeScore #-}
getNodeScore :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Score -> Depth -> Ply -> Position -> IO (Score, Maybe Move)
getNodeScore !alpha !beta !depth !ply pos
  | depth == 0 = let !score = quiesceSearch alpha beta 0 pos
                in pure (score, Nothing)
  | otherwise = do
  moves <- getSortedMoves depth ply pos
  (!score, (!newAlpha, !bestMove)) <-
    runStateT (getMovesScore beta depth ply pos moves) (alpha, Nothing)
  pure (fromMaybe newAlpha score, bestMove)


{-# INLINE  getMovesScore #-}
getMovesScore :: (?killersTable :: KillersTable, ?tTable::TTable)
  => Score -> Depth -> Ply -> Position -> ([Move], [Move]) -> SearchM (Maybe Score)
getMovesScore !beta !depth !ply pos (mainMoves, reducedMoves) = do
  mainSearchScore      <- mainMovesSearch
  (mainSearchAlpha, _) <- get
  reducedSearchScore <- if isJust mainSearchScore
                         then pure Nothing
                         else reducedMovesSearch mainSearchAlpha
  if isJust reducedSearchScore
    then reducedMovesFullSearch
    else pure mainSearchScore
  where
    mainMovesSearch          = movesSearch beta depth mainMoves
    reducedMovesSearch alpha = movesSearch (alpha + 1)
                                           (depth - 1)
                                           reducedMoves
    reducedMovesFullSearch   = movesSearch beta depth reducedMoves
    movesSearch beta' depth' = findTraverse
      (getMoveScore beta' depth' ply pos)


{-# INLINE  getMoveScore #-}
getMoveScore :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Score -> Depth -> Ply -> Position -> Move -> SearchM (Maybe Score)
getMoveScore !beta !depth !ply !pos !move =
  do !alpha <- gets fst
     !score <- negate <$> liftIO (negamax (-beta) (-alpha)
                                         (depth - 1)
                                         (ply + 1)
                                         (makeMove move pos))
     let !nodeType = getNodeType alpha beta score
     advanceState beta score ply move pos nodeType


{-# INLINE  advanceState #-}
advanceState :: (?killersTable :: KillersTable)
  => Score -> Score -> Ply -> Move -> Position -> NodeType -> SearchM (Maybe Score)
advanceState !beta !score !ply !move !pos !nodeType =
  case nodeType of
    PV  -> put (score, Just move)
            $> Nothing
    Cut -> modify' (second (const $ Just move))
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
