module Search.Quiescence (quiesceSearch) where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove

import           Evaluation.Evaluation

import           Control.Monad.State
import           MoveGen.PieceCaptures (allCaptures)


quiesceSearch :: (?nodes :: IORef Word64)
  => Score -> Score -> Ply -> Position -> IO Score
quiesceSearch !alpha !beta !ply !pos
  | standPat >= beta = pure beta
  | otherwise       = do
      modifyIORef' ?nodes (+ 1)
      (score, finalAlpha) <- runStateT scoreState newAlpha
      pure $! fromMaybe finalAlpha score
  where
    scoreState = findTraverseIndex (getMoveScore beta ply pos) captures
    captures   = getWinningCaptures pos
    !newAlpha = max alpha standPat
    !standPat = evaluatePosition pos


getMoveScore :: (?nodes :: IORef Word64)
  => Score -> Ply -> Position -> Int -> Move -> QuiesceM (Maybe Score)
getMoveScore !beta !ply !pos _ mv =
  do !alpha <- get
     !score <- liftIO (negate <$> quiesceSearch (-beta) (-alpha) (ply + 1)
                                  (makeMove mv pos))
     let !nodeType = getNodeType alpha beta score
     advanceState beta score nodeType


advanceState :: Score -> Score -> NodeType -> QuiesceM (Maybe Score)
advanceState !beta !score !nodeType =
  case nodeType of
    PV  -> Nothing <$ put score
    Cut -> pure $ Just beta
    All -> pure Nothing


getWinningCaptures :: Position -> [Move]
getWinningCaptures pos =
  filter ((>= 0) . (`evaluateCaptureExchange` pos))
    $ allCaptures pos

type QuiesceM = StateT Score IO
