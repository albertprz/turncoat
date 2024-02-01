module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Models.Position
import           Models.Score
import           MoveGen.PieceAttacks


evaluatePosition :: Position -> Score
evaluatePosition pos@Position {..} =
  materialScore
    + threatFactor  * fromIntegral (playerThreats - enemyThreats)
  where
    !playerThreats  = ones (playerMoves & enemy .\ enemyMoves)
    !enemyThreats   = ones (enemyMoves & player .\ playerMoves)
    !playerMoves    = allPlayerAttacks pos
    !enemyMoves     = attacked

threatFactor :: Score
threatFactor = 20
