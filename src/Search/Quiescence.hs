module Search.Quiescence (quiesceSearch) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove

import           Control.Monad.State
import           Models.Move
import           Search.MoveOrdering


{-# INLINE  quiesceSearch #-}
quiesceSearch :: Score -> Score -> Position -> Score
quiesceSearch !alpha !beta !pos =
  if | standPat >= beta -> beta
     | null moves       -> realAlpha
     | otherwise        -> fromMaybe newAlpha score
  where
    (score, newAlpha) = runState scoreState realAlpha
    scoreState = findTraverse (getMoveScore beta pos) moves
    moves = getSortedCaptures pos
    realAlpha = max alpha standPat
    !standPat = evaluatePosition pos


{-# INLINE  getMoveScore #-}
getMoveScore :: Score -> Position -> Move -> State Score (Maybe Score)
getMoveScore !beta !pos !move =
  do !alpha <- get
     let score = - quiesceSearch (-beta) (-alpha)
                                  (makeMove move pos)
         nodeType = getNodeType alpha beta score
     advanceState beta score nodeType

{-# INLINE  advanceState #-}
advanceState :: Score -> Score -> NodeType -> State Score (Maybe Score)
advanceState !beta !score !nodeType =
  case nodeType of
    PV  -> Nothing          <$ put score
    Cut -> pure $ Just beta
    All -> pure Nothing
