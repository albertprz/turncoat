module Search.Quiescence (quiesceSearch) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove

import           Control.Monad.State
import           Models.Move
import           MoveGen.PieceCaptures (allLegalCaptures)
import           Search.MoveOrdering


{-# INLINE  quiesceSearch #-}
quiesceSearch :: Score -> Score -> Ply -> Position -> Score
quiesceSearch !alpha !beta !ply !pos
  | ply == 8         = standPat
  | standPat >= beta = beta
  | otherwise       = fromMaybe newAlpha score
  where
    (score, newAlpha) = runState scoreState realAlpha
    scoreState = findTraverse (getMoveScore beta ply pos) captures
    realAlpha = max alpha standPat
    captures  | ply <= 1   = fst $ getSortedCaptures pos
              | otherwise = allLegalCaptures pos
    !standPat  = evaluatePosition pos


{-# INLINE  getMoveScore #-}
getMoveScore :: Score -> Ply -> Position -> Move -> State Score (Maybe Score)
getMoveScore !beta !ply !pos !move =
  do !alpha <- get
     let !score = - quiesceSearch (-beta) (-alpha) (ply + 1)
                                  (makeMove move pos)
         !nodeType = getNodeType alpha beta score
     advanceState beta score nodeType


{-# INLINE  advanceState #-}
advanceState :: Score -> Score -> NodeType -> State Score (Maybe Score)
advanceState !beta !score !nodeType =
  case nodeType of
    PV  -> Nothing          <$ put score
    Cut -> pure $ Just beta
    All -> pure Nothing
