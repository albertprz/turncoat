module Search.Search where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TEntry (..), TTable, ZKey)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           Search.MoveOrdering

import           Control.Monad.State


{-# INLINE  getBestMove #-}
getBestMove :: (?tTable :: TTable) => Depth -> Position -> IO (Maybe Move)
getBestMove !depth !pos = do
  moves <- getSortedMoves depth pos
  let scoreState = findTraverse (getMoveScore initialBeta depth pos)
                                moves
  snd <$> execStateT scoreState (initialAlpha, Nothing)


initialAlpha :: Score
initialAlpha = minBound + 1

initialBeta :: Score
initialBeta = maxBound - 1

{-# INLINE  negamax #-}
negamax :: (?tTable :: TTable) => Score -> Score -> Depth -> Position -> IO Score
negamax !alpha !beta !depth !pos
  | depth == 0 = pure $! evaluatePosition pos
  | otherwise = do
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
getNodeScore !alpha !beta !depth !pos = do
  moves <- getSortedMoves depth pos
  let scoreState = findTraverse (getMoveScore beta depth pos)
                                moves
  (!score, (!newAlpha, !bestMove)) <- runStateT scoreState
                                               (alpha, Nothing)
  pure (fromMaybe newAlpha score, bestMove)

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
advanceState beta score move nodeType =
  case nodeType of
    Cut -> Just beta    <$ modify' (second (const $ Just move))
    PV  -> Nothing      <$ put (score, Just move)
    All -> pure Nothing


{-# INLINE  getNodeType #-}
getNodeType :: Score -> Score -> Score -> NodeType
getNodeType alpha beta score
  | score >= beta  = Cut
  | score > alpha = PV
  | otherwise     = All

type SearchM = StateT (Score, Maybe Move) IO
