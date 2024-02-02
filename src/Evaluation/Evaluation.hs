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
    - evaluatePieceThreats player playerAttacks enemyAttacks
    + evaluatePieceThreats enemy  enemyAttacks  playerAttacks
    - evaluateKingSafety   player kings         enemyAttacks
    + evaluateKingSafety   enemy  kings         playerAttacks
  where
    !playerAttacks  = allPlayerAttacks pos
    !enemyAttacks   = attacked


{-# INLINE  evaluatePieceThreats #-}
evaluatePieceThreats :: Board -> Board -> Board -> Score
evaluatePieceThreats player playerAttacks enemyAttacks =
 threatFactor * fromIntegral threats
  where
    !threats = ones (enemyAttacks & player .\ playerAttacks)


{-# INLINE  evaluateKingSafety #-}
evaluateKingSafety :: Board -> Board -> Board -> Score
evaluateKingSafety player kings enemyAttacks
  | kingMoves == 0 = 0
  | otherwise = kingUnsafety + kingUnsafeArea
  where
    kingUnsafety   = (kingSafetyFactor
           * fromIntegral (ones kingUnsafeSquares))
       `div` fromIntegral (ones kingMoves)
    kingUnsafeArea = kingSquareSafetyFactor
     * fromIntegral (ones kingUnsafeSquares)
    !kingMoves         = kingAttacks king .\ player
    !kingUnsafeSquares = kingMoves & enemyAttacks
    !king              = lsb (player & kings)


threatFactor :: Score
threatFactor = 20

kingSafetyFactor :: Score
kingSafetyFactor = 100

kingSquareSafetyFactor :: Score
kingSquareSafetyFactor = 20
