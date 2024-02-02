module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Models.Position
import           Models.Score
import           MoveGen.PieceAttacks


{-# INLINE  evaluatePosition #-}
evaluatePosition :: Position -> Score
evaluatePosition pos@Position {..} =
  materialScore
    + evaluatePieceThreats enemy  enemyAttacks  playerAttacks
    - evaluatePieceThreats player playerAttacks enemyAttacks
    + evaluateKingSafety   player kings         enemyAttacks
    - evaluateKingSafety   enemy  kings         playerAttacks
  where
    !playerAttacks  = allPlayerAttacks pos
    !enemyAttacks   = attacked


{-# INLINE  evaluatePieceThreats #-}
evaluatePieceThreats :: Board -> Board -> Board -> Score
evaluatePieceThreats enemy enemyAttacks playerAttacks =
 threatFactor * fromIntegral threats
  where
    !threats = ones (playerAttacks & enemy .\ enemyAttacks)


{-# INLINE  evaluateKingSafety #-}
evaluateKingSafety :: Board -> Board -> Board -> Score
evaluateKingSafety player kings enemyAttacks
  | kingMoves == 0 = 0
  | otherwise = (kingSafetyFactor
        * fromIntegral (ones kingUnsafeSquares))
    `div` fromIntegral (ones kingMoves)
  where
    !kingMoves         = kingAttacks king .\ player
    !kingUnsafeSquares = kingMoves & enemyAttacks
    !king              = lsb (player & kings)


threatFactor :: Score
threatFactor = 20

kingSafetyFactor :: Score
kingSafetyFactor = 100
