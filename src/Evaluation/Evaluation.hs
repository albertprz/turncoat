module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Evaluation.Material
import           Models.Position
import           Models.Score
import           MoveGen.PieceAttacks


{-# INLINE  evaluatePosition #-}
evaluatePosition :: Position -> Score
evaluatePosition pos@Position {..} =
  materialScore
    - evaluatePieceThreats player playerAttacks enemyAttacks pos
    + evaluatePieceThreats enemy  enemyAttacks  playerAttacks pos
    - evaluateKingSafety   player kings         enemyAttacks
    + evaluateKingSafety   enemy  kings         playerAttacks
    + min 0 (onesScore (player & bishops) - 1) * bishopPairBonus
    - min 0 (onesScore (enemy  & bishops) - 1) * bishopPairBonus
  where
    !playerAttacks  = allPlayerAttacks pos
    !enemyAttacks   = attacked


{-# INLINE  evaluatePieceThreats #-}
evaluatePieceThreats :: Board -> Board -> Board -> Position -> Score
evaluatePieceThreats defender defenderAttacks attackerAttacks Position {..} =
 threatFactor * threatenedScore `div` pawnScore
  where
    !threatenedScore =
        onesScore (threatened & pawns)   * pawnScore
      + onesScore (threatened & knights) * knightScore
      + onesScore (threatened & bishops) * bishopScore
      + onesScore (threatened & rooks)   * rookScore
      + onesScore (threatened & queens)  * queenScore
    !threatened     =
      attackerAttacks & defender .\ defenderAttacks


{-# INLINE  evaluateKingSafety #-}
evaluateKingSafety :: Board -> Board -> Board -> Score
evaluateKingSafety defender attackerAttacks kings
  | kingMoves == 0 = 0
  | otherwise = kingUnsafety + kingUnsafeArea
  where
    kingUnsafety   = (kingSafetyFactor
           * onesScore kingUnsafeSquares)
       `div` onesScore kingMoves
    kingUnsafeArea = kingSquareSafetyFactor
           * onesScore kingUnsafeSquares
    !kingMoves         = kingAttacks king .\ defender
    !kingUnsafeSquares = kingMoves & attackerAttacks
    !king              = lsb (defender & kings)


{-# INLINE  onesScore #-}
onesScore :: Board -> Score
onesScore = fromIntegral . ones


threatFactor :: Score
threatFactor = 20

kingSafetyFactor :: Score
kingSafetyFactor = 100

kingSquareSafetyFactor :: Score
kingSquareSafetyFactor = 20

bishopPairBonus :: Score
bishopPairBonus = 40
