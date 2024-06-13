module Search.Quiescence (quiesceSearch) where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove

import           Evaluation.Evaluation

import           Control.Monad.State
import           MoveGen.PieceCaptures (allCaptures)


quiesceSearch :: Score -> Score -> Ply -> Position -> Score
quiesceSearch !alpha !beta !ply !pos
  | standPat >= beta = beta
  | otherwise       = fromMaybe finalAlpha score
  where
    (score, finalAlpha) = runState scoreState newAlpha
    scoreState = findTraverseIndex (getMoveScore beta ply pos) captures
    captures   = getWinningCaptures pos

    !newAlpha = max alpha standPat
    !standPat = evaluatePosition pos

getMoveScore :: Score -> Ply -> Position -> Int -> Move -> State Score (Maybe Score)
getMoveScore !beta !ply !pos _ mv =
  do !alpha <- get
     let !score = - quiesceSearch (-beta) (-alpha) (ply + 1)
                                  (makeMove mv pos)
         !nodeType = getNodeType alpha beta score
     advanceState beta score nodeType


advanceState :: Score -> Score -> NodeType -> State Score (Maybe Score)
advanceState !beta !score !nodeType =
  case nodeType of
    PV  -> Nothing          <$ put score
    Cut -> pure $ Just beta
    All -> pure Nothing


getWinningCaptures :: Position -> [Move]
getWinningCaptures pos =
  filter ((>= 0) . (`evaluateCaptureExchange` pos))
    $ allCaptures pos
