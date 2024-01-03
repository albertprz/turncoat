module Search.Search where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TEntry (..), TTable, ZKey)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           MoveGen.PieceMoves

import           Control.Monad.State


{-# INLINE  getBestMove #-}
getBestMove :: (?tTable :: TTable) => Int -> Position -> IO (Maybe Move)
getBestMove !depth !pos =
  snd <$> execStateT scoreState (initialAlpha, Nothing)
  where
    scoreState = findTraverse (getMoveScore initialBeta depth pos)
                              moves
    moves      = toList (allLegalMoves pos)

{-# INLINE  initialAlpha #-}
initialAlpha :: Score
initialAlpha = -1000

{-# INLINE  initialBeta #-}
initialBeta :: Score
initialBeta = 1000

{-# INLINE  negamax #-}
negamax :: (?tTable :: TTable) => Score -> Score -> Int -> Position -> SearchM Score
negamax !alpha !beta !depth !pos
  | depth == 0 = pure $! evaluatePosition pos
  | depth <= 2 = getNodeScore alpha beta depth pos
  | otherwise = do
    let !zKey = getZobristKey pos
    !tEntry <- liftIO $ TTable.lookup depth zKey
    case tEntry of
      Just entry -> pure $! entry.score
      Nothing    -> cacheNodeScore alpha beta depth pos zKey

{-# INLINE  cacheNodeScore #-}
cacheNodeScore :: (?tTable:: TTable) => Score -> Score -> Int -> Position -> ZKey -> SearchM Score
cacheNodeScore !alpha !beta !depth !pos !zKey = do
  !score <- getNodeScore alpha beta depth pos
  !bestMove <- gets snd
  let
    !nodeType = getNodeType alpha beta score
    !newTEntry = TEntry {
      depth = depth,
      bestMove = bestMove,
      score = score,
      nodeType = nodeType
    }
  liftIO $ TTable.insert zKey newTEntry
  pure $! score

{-# INLINE  getNodeScore #-}
getNodeScore :: (?tTable :: TTable) => Score -> Score -> Int -> Position -> SearchM Score
getNodeScore !alpha !beta !depth !pos = do
  (!score, (!newAlpha, _)) <- liftIO $ runStateT scoreState
                                    (alpha, Nothing)
  pure $! fromMaybe newAlpha score
  where
    scoreState = findTraverse (getMoveScore beta depth pos)
                              moves
    moves      = toList (allLegalMoves pos)

{-# INLINE  getMoveScore #-}
getMoveScore :: (?tTable :: TTable) => Score -> Int -> Position -> Move -> SearchM (Maybe Score)
getMoveScore !beta !depth !pos !move =
  do !alpha <- gets fst
     !score <- negate <$> negamax (-beta) (-alpha) (depth - 1)
                           (makeMove move pos)
     if | score >= beta -> pure (Just beta)
        | score > alpha -> put (score, Just move) $> Nothing
        | otherwise     -> pure Nothing

{-# INLINE  getNodeType #-}
getNodeType :: Score -> Score -> Score -> NodeType
getNodeType !alpha !beta !score
  | score >= beta  = Cut
  | score > alpha = PV
  | otherwise     = All


type SearchM = StateT (Score, Maybe Move) IO
