module Evaluation.Evaluation (evaluateCaptureExchange, evaluatePosition) where

import           AppPrelude

import           Constants.Boards
import           Evaluation.Material
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.PieceAttacks
import           MoveGen.PieceCaptures
import           MoveGen.PositionQueries (isKingInCheck)


evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange initialMv@Move {..} initialPos =
  evaluateExchange end (makeMove initialMv initialPos)
    - initialPos.materialScore
  where
    evaluateExchange !square pos =
      case headMay $ staticExchangeCaptures square pos of
        Just mv -> - (max pos.materialScore
                        (evaluateExchange square (makeMove mv pos)))
        Nothing -> - pos.materialScore


evaluatePosition :: Position -> Score
evaluatePosition pos@Position {..} =
  materialScore
  + evaluatePositionHelper enemyPos.attacked enemyPos.mobilityScore pos
  - evaluatePositionHelper pos.attacked      pos.mobilityScore      enemyPos
  where
    enemyPos = makeFastNullMove pos


evaluatePositionHelper :: Board -> Score -> Position -> Score
evaluatePositionHelper playerAttacked mobilityScore pos =
    evaluatePositionBonuses mobilityScore pos
  - evaluatePositionMaluses playerAttacked pos


evaluatePositionBonuses :: Score -> Position -> Score
evaluatePositionBonuses mobilityScore pos =
  evaluateMobility mobilityScore pos
  + evaluateBishopPair pos
  -- evaluate Passed pawns
  -- evaluate knight outposts

evaluateMobility :: Score -> Position -> Score
evaluateMobility mobilityScore pos
  | isKingInCheck pos = 0
  | otherwise         = mobilityScore


evaluatePositionMaluses :: Board -> Position -> Score
evaluatePositionMaluses playerAttacked pos@Position {..} =
  evaluateKingSafety   player   kings          attacked
  + evaluatePieceThreats player playerAttacked attacked pos
  + evaluateDoubledPawns  (player & pawns)
  + evaluateIsolatedPawns (player & pawns)


evaluateBishopPair :: Position -> Score
evaluateBishopPair Position {..} =
   bishopPairBonus * max 0 (onesScore (player & bishops) - 1)


evaluateKingSafety :: Board -> Board -> Board -> Score
evaluateKingSafety defender kings attackerAttacks =
  kingSafetySquareMalus * onesScore kingUnsafeSquares
  where
    kingMoves         = kingAttacks king .\ defender
    kingUnsafeSquares = kingMoves & attackerAttacks
    king             = lsb (defender & kings)


evaluatePieceThreats :: Board -> Board -> Board -> Position -> Score
evaluatePieceThreats defender defenderAttacks attackerAttacks Position {..} =
 (pieceThreatMalus * threatenedScore) `div` pawnScore
  where
    threatenedScore =
        onesScore (threatened & knights) * knightScore
      + onesScore (threatened & bishops) * bishopScore
      + onesScore (threatened & rooks)   * rookScore
      + onesScore (threatened & queens)  * queenScore
    threatened     =
      attackerAttacks & defender .\ defenderAttacks


evaluateDoubledPawns :: Board -> Score
evaluateDoubledPawns pawns =
  doubledPawnMalus * doubledPawnsCount
  where
    doubledPawnsCount =
        max 0 (onesScore (file_A & pawns) - 1)
      + max 0 (onesScore (file_B & pawns) - 1)
      + max 0 (onesScore (file_C & pawns) - 1)
      + max 0 (onesScore (file_D & pawns) - 1)
      + max 0 (onesScore (file_E & pawns) - 1)
      + max 0 (onesScore (file_F & pawns) - 1)
      + max 0 (onesScore (file_G & pawns) - 1)
      + max 0 (onesScore (file_H & pawns) - 1)


evaluateIsolatedPawns :: Board -> Score
evaluateIsolatedPawns pawns =
  isolatedPawnMalus * isolatedPawnsCount
  where
    isolatedPawnsCount =
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


onesScore :: Board -> Score
onesScore !x = fromIntegral $! ones x


bishopPairBonus :: Score
bishopPairBonus = 50

kingSafetySquareMalus :: Score
kingSafetySquareMalus = 40

pieceThreatMalus :: Score
pieceThreatMalus = 20

doubledPawnMalus :: Score
doubledPawnMalus = 25

isolatedPawnMalus :: Score
isolatedPawnMalus = 25
