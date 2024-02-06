module Evaluation.Evaluation where

import           AppPrelude

import           Constants.Boards
import           Evaluation.Material
import           Evaluation.PieceTables
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.PieceAttacks


{-# INLINE  evaluatePosition #-}
evaluatePosition :: Position -> Score
evaluatePosition pos@Position {..} =
  materialScore
  + evaluatePositionBonuses pos
  - evaluatePositionMaluses pos


{-# INLINE  evaluatePositionBonuses #-}
evaluatePositionBonuses :: Position -> Score
evaluatePositionBonuses pos@Position {..} =

    evaluatePieceMobility player enemy pos
  - evaluatePieceMobility enemy player pos

  + max 0 (onesScore (player & bishops) - 1) * bishopPairBonus
  - max 0 (onesScore (enemy  & bishops) - 1) * bishopPairBonus


{-# INLINE  evaluatePositionMaluses #-}
evaluatePositionMaluses :: Position -> Score
evaluatePositionMaluses pos@Position {..} =

    evaluateKingSafety   player kings         enemyAttacks
  - evaluateKingSafety   enemy  kings         playerAttacks

  + evaluatePieceThreats player playerAttacks enemyAttacks pos
  - evaluatePieceThreats enemy  enemyAttacks  playerAttacks pos

  + evaluateDoubledPawns  (player & pawns)
  - evaluateDoubledPawns  (enemy  & pawns)

  + evaluateIsolatedPawns (player & pawns)
  - evaluateIsolatedPawns (enemy  & pawns)

  where
    !playerAttacks  = allPlayerAttacks pos
    !enemyAttacks   = attacked


{-# INLINE  evaluateKingSafety #-}
evaluateKingSafety :: Board -> Board -> Board -> Score
evaluateKingSafety defender attackerAttacks kings
  | kingMoves == 0 = 0
  | otherwise = kingUnsafety + kingUnsafeArea
  where
    kingUnsafety   = (kingSafetyMalus
           * onesScore kingUnsafeSquares)
       `div` onesScore kingMoves
    kingUnsafeArea = kingSafetySquareMalus
           * onesScore kingUnsafeSquares
    !kingMoves         = kingAttacks king .\ defender
    !kingUnsafeSquares = kingMoves & attackerAttacks
    !king              = lsb (defender & kings)


{-# INLINE  evaluatePieceMobility #-}
evaluatePieceMobility :: Board -> Board -> Position -> Score
evaluatePieceMobility player enemy
    Position {knights, bishops, rooks, queens} =
  knightsMobility + bishopsMobility + rooksMobility + queensMobility
  where
    !knightsMobility =
      foldlBoard 0 (+)
        (getMobilityScore knightMobilityTable . knightAttacks)
        (player & knights)
    !bishopsMobility =
      foldlBoard 0 (+)
        (getMobilityScore bishopMobilityTable . bishopAttacks allPieces)
        (player & bishops)
    !rooksMobility =
      foldlBoard 0 (+)
        (getMobilityScore rookMobilityTable . rookAttacks allPieces)
        (player & rooks)
    !queensMobility =
      foldlBoard 0 (+)
        (getMobilityScore queenMobilityTable . queenAttacks allPieces)
        (player & queens)

    getMobilityScore !table = (table !!) . ones . (.\ player)
    !allPieces = player .| enemy


{-# INLINE  evaluatePieceThreats #-}
evaluatePieceThreats :: Board -> Board -> Board -> Position -> Score
evaluatePieceThreats defender defenderAttacks attackerAttacks Position {..} =
 pieceThreatMalus * threatenedScore `div` pawnScore
  where
    !threatenedScore =
        onesScore (threatened & pawns)   * pawnScore
      + onesScore (threatened & knights) * knightScore
      + onesScore (threatened & bishops) * bishopScore
      + onesScore (threatened & rooks)   * rookScore
      + onesScore (threatened & queens)  * queenScore
    !threatened     =
      attackerAttacks & defender .\ defenderAttacks


{-# INLINE  evaluateDoubledPawns #-}
evaluateDoubledPawns :: Board -> Score
evaluateDoubledPawns pawns =
  doubledPawnMalus * doubledPawnsCount
  where
    !doubledPawnsCount =
        max 0 (onesScore (file_A & pawns) - 1)
      + max 0 (onesScore (file_B & pawns) - 1)
      + max 0 (onesScore (file_C & pawns) - 1)
      + max 0 (onesScore (file_D & pawns) - 1)
      + max 0 (onesScore (file_E & pawns) - 1)
      + max 0 (onesScore (file_F & pawns) - 1)
      + max 0 (onesScore (file_G & pawns) - 1)
      + max 0 (onesScore (file_H & pawns) - 1)


{-# INLINE  evaluateIsolatedPawns #-}
evaluateIsolatedPawns :: Board -> Score
evaluateIsolatedPawns pawns =
  isolatedPawnMalus * isolatedPawnsCount
  where
    !isolatedPawnsCount =
      onesScore (file_B & pawns)
        * (1 - min 1 (onesScore (pawns & (file_A .| file_C))))
      + onesScore (file_C & pawns)
        * (1 - min 1 (onesScore (pawns & (file_B .| file_D))))
      + onesScore (file_D & pawns)
        * (1 - min 1 (onesScore (pawns & (file_C .| file_E))))
      + onesScore (file_E & pawns)
        * (1 - min 1 (onesScore (pawns & (file_D .| file_F))))
      + onesScore (file_F & pawns)
        * (1 - min 1 (onesScore (pawns & (file_E .| file_G))))
      + onesScore (file_G & pawns)
        * (1 - min 1 (onesScore (pawns & (file_F .| file_H))))


{-# INLINE  onesScore #-}
onesScore :: Board -> Score
onesScore = fromIntegral . ones


bishopPairBonus :: Score
bishopPairBonus = 40

kingSafetyMalus :: Score
kingSafetyMalus = 100

kingSafetySquareMalus :: Score
kingSafetySquareMalus = 20

pieceThreatMalus :: Score
pieceThreatMalus = 10

doubledPawnMalus :: Score
doubledPawnMalus = 25

isolatedPawnMalus :: Score
isolatedPawnMalus = 25
