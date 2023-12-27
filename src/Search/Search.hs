module Search.Search where

import           AppPrelude

import           Evaluation.Evaluation
import           Evaluation.Score
import           Models.Move
import           Models.Position
import           MoveGen.MakeMove
import           MoveGen.PieceMoves

import           Control.Monad.State


{-# INLINE  getBestMove #-}
getBestMove :: Int -> Position -> Maybe Move
getBestMove !depth !pos =
   snd $ execState scoreState (initialAlpha, Nothing)
     where
     scoreState =
       findTraverse (moveScore initialBeta depth pos) moves
     moves      = toList (allLegalMoves pos)

{-# INLINE  initialAlpha #-}
initialAlpha :: Score
initialAlpha = -200

{-# INLINE  initialBeta #-}
initialBeta :: Score
initialBeta = 200

{-# INLINE  negamax #-}
negamax :: Score -> Score -> Int -> Position -> Score
negamax !alpha !beta !depth !pos
  | depth == 0 = evaluatePosition pos
  | otherwise = fromMaybe newAlpha score
    where
    (!score, (!newAlpha, _)) =
      runState scoreState (alpha, Nothing)
    scoreState =
      findTraverse (moveScore beta depth pos) moves
    moves      = toList (allLegalMoves pos)

{-# INLINE  moveScore #-}
moveScore :: Score -> Int -> Position -> Move -> State (Score, Maybe Move) (Maybe Score)
moveScore !beta !depth !pos !move =
  do !alpha <- gets fst
     let !score = -negamax (-beta) (-alpha) (depth - 1) (playMove move pos)
     if | score >= beta -> pure (Just beta)
        | score > alpha -> put (score, Just move) $> Nothing
        | otherwise     -> pure Nothing


{-# INLINE  findTraverse #-}
findTraverse :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
findTraverse !f (!x : xs) =
  do !ret <- f x
     maybe (findTraverse f xs) (pure . Just) ret

findTraverse _ [] =
  pure Nothing
