module Evaluation.Evaluation (evaluateCaptureExchange, evaluatePosition) where

import           AppPrelude

import           Evaluation.Constants
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.PieceCaptures
import           MoveGen.PositionQueries
import           Utils.Board

import           Data.Ord                (clamp)


evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange initialMv@Move {..} initialPos =
  evaluateExchange end (makeMove initialMv initialPos)
    - initialPos.materialScore
  where
    evaluateExchange !square pos =
      case headMay $ staticExchangeCaptures square pos of
        Just mv -> - (max pos.materialScore
                        $! evaluateExchange square (makeMove mv pos))
        Nothing -> - pos.materialScore


evaluatePosition :: Position -> Score
evaluatePosition pos =
  pos.materialScore
  + evaluatePositionHelper pos
  - evaluatePositionHelper enemyPos
  where
    enemyPos = makeNullMove pos


evaluatePositionHelper :: Position -> Score
evaluatePositionHelper pos =
    evaluatePositionBonuses pos
  - evaluatePositionPenalties pos


evaluatePositionBonuses :: Position -> Score
evaluatePositionBonuses pos =
  evaluateMobility pos
  + evaluateBishopPair pos
  -- evaluate Passed pawns
  -- evaluate knight outposts

evaluatePositionPenalties :: Position -> Score
evaluatePositionPenalties pos@Position {..} =
  evaluateKingSafety pos
  + evaluateIsolatedPawns (player & pawns)


evaluateMobility :: Position -> Score
evaluateMobility pos
  | isKingInCheck pos = 0
  | otherwise         = pos.mobilityScore


evaluateBishopPair :: Position -> Score
evaluateBishopPair Position {..} =
   bishopPairBonus * clamp (0, 1) (onesScore (player & bishops) - 1)


evaluateKingSafety :: Position -> Score
evaluateKingSafety pos =
  pos.kingSafetyScore


evaluateIsolatedPawns :: Board -> Score
evaluateIsolatedPawns pawns =
  isolatedPawnPenalty * fromIntegral isolatedPawnsCount
  where
    isolatedPawnsCount =
      onesBoard (file_B & pawns)
        * toCondition (pawns & (file_A .| file_C))
      + onesBoard (file_C & pawns)
        * toCondition (pawns & (file_B .| file_D))
      + onesBoard (file_D & pawns)
        * toCondition (pawns & (file_C .| file_E))
      + onesBoard (file_E & pawns)
        * toCondition (pawns & (file_D .| file_F))
      + onesBoard (file_F & pawns)
        * toCondition (pawns & (file_E .| file_G))
      + onesBoard (file_G & pawns)
        * toCondition (pawns & (file_F .| file_H))


onesBoard :: Board -> Board
onesBoard !x = fromIntegral $! ones x

onesScore :: Board -> Score
onesScore !x = fromIntegral $! ones x
