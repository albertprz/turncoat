module Search.Search (getBestMove) where

import           AppPrelude

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
getBestMove :: (?tTable::TTable) => Depth -> Position -> IO (Maybe Move)
getBestMove !depth !pos =
  lastEx <$> evalStateT (traverse (`aspirationSearch` pos) [0 .. depth])
                        (initialAlpha, initialBeta)


{-# INLINE  aspirationSearch #-}
aspirationSearch :: (?tTable::TTable) => Depth -> Position -> AspirationSearchM (Maybe Move)
aspirationSearch !depth pos  = do
  (!alpha, !beta) <- get
  (!score, !mv) <- liftIO $ getNodeScore alpha beta depth pos
  case getNodeType alpha beta score of
    PV  -> put (score - windowDelta, score + windowDelta) $> mv
    Cut -> modify (second (+ windowDelta))       *> aspirationSearch depth pos
    All -> modify (first (\x -> x - windowDelta)) *> aspirationSearch depth pos


{-# INLINE  negamax #-}
negamax :: (?tTable :: TTable) => Score -> Score -> Depth -> Position -> IO Score
negamax !alpha !beta !depth pos = do
    let !zKey = getZobristKey pos
    !ttScore <- liftIO $ TTable.lookupScore alpha beta depth zKey
    case ttScore of
      Just !score -> pure score
      Nothing     -> cacheNodeScore alpha beta depth pos zKey


{-# INLINE  cacheNodeScore #-}
cacheNodeScore :: (?tTable:: TTable) => Score -> Score -> Depth -> Position -> ZKey -> IO Score
cacheNodeScore !alpha !beta !depth !pos !zKey = do
  (!score, !bestMove) <- getNodeScore alpha beta depth pos
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
getNodeScore :: (?tTable :: TTable) => Score -> Score -> Depth -> Position
  -> IO (Score, Maybe Move)
getNodeScore !alpha !beta !depth pos
  | depth == 0 = let !score = quiesceSearch alpha beta 0 pos
                in pure (score, Nothing)
  | otherwise = do
  moves <- getSortedMoves depth pos
  (!score, (!newAlpha, !bestMove)) <-
    runStateT (getMovesScore depth beta pos moves) (alpha, Nothing)
  pure (fromMaybe newAlpha score, bestMove)


{-# INLINE  getMovesScore #-}
getMovesScore :: (?tTable::TTable) => Depth -> Score -> Position -> ([Move], [Move]) -> SearchM (Maybe Score)
getMovesScore !depth !beta pos (mainMoves, reducedMoves) = do
  mainSearchScore      <- mainMovesSearch
  (mainSearchAlpha, _) <- get
  reducedSearchScore <- if isJust mainSearchScore then
                         pure Nothing
                       else
                         reducedMovesSearch mainSearchAlpha
  if isJust reducedSearchScore then
    reducedMovesFullSearch
  else
    pure mainSearchScore
  where
    mainMovesSearch          = movesSearch depth beta mainMoves
    reducedMovesSearch alpha = movesSearch (depth - 1) (alpha + 1)
                                                       reducedMoves
    reducedMovesFullSearch   = movesSearch depth beta reducedMoves
    movesSearch depth' beta' = findTraverse (getMoveScore beta' depth' pos)


{-# INLINE  getMoveScore #-}
getMoveScore :: (?tTable :: TTable) => Score -> Depth -> Position -> Move
  -> SearchM (Maybe Score)
getMoveScore !beta !depth !pos !move =
  do !alpha <- gets fst
     !score <- negate <$> liftIO (negamax (-beta) (-alpha) (depth - 1)
                                (makeMove move pos))
     let !nodeType = getNodeType alpha beta score
     advanceState beta score move nodeType


{-# INLINE  advanceState #-}
advanceState :: Score -> Score -> Move -> NodeType -> SearchM (Maybe Score)
advanceState !beta !score !move !nodeType =
  case nodeType of
    PV  -> Nothing      <$ put (score, Just move)
    Cut -> Just beta    <$ modify' (second (const $ Just move))
    All -> pure Nothing


initialAlpha :: Score
initialAlpha = minBound + 1

initialBeta :: Score
initialBeta = maxBound - 1

windowDelta :: Score
windowDelta = 10


type SearchM           = StateT (Score, Maybe Move) IO
type AspirationSearchM = StateT (Score, Score) IO
