module Evaluation.Evaluation (evaluateCaptureExchange, evaluatePosition,
getScoreBreakdown, ScoreBreakdown)
where

import           AppPrelude

import           Bookhound.Utils.Text    (indent)
import           Evaluation.Constants
import           Evaluation.Material
import           Models.Move
import           Models.Piece
import           Models.Position
import           Models.Score
import           MoveGen.MakeMove
import           MoveGen.PieceAttacks
import           MoveGen.PieceCaptures
import           MoveGen.PositionQueries
import           Utils.Board


getScoreBreakdown :: Position -> ScoreBreakdown
getScoreBreakdown pos@Position{..} = ScoreBreakdown {..}
  where
    (white, black)
      | color == White = (playerBreakdown, enemyBreakdown)
      | otherwise     = (enemyBreakdown, playerBreakdown)
    playerBreakdown = (evaluatePositionBreakdown pos)
      { material = evaluatePlayerMaterial pos player color }
    enemyBreakdown  = (evaluatePositionBreakdown enemyPos)
      { material = evaluatePlayerMaterial pos enemy (reverseColor color) }
    enemyPos        = makeNullMove pos


evaluateCaptureExchange :: Move -> Position -> Score
evaluateCaptureExchange initialMv initialPos =
  evaluateExchange initialMv.end (makeMove initialMv initialPos)
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
  + evalScore (evaluatePositionBreakdown pos)
  - evalScore (evaluatePositionBreakdown enemyPos)
  where
    enemyPos = makeNullMove pos


evaluatePositionBreakdown :: Position -> PlayerScoreBreakdown
evaluatePositionBreakdown pos =
  PlayerScoreBreakdown {
    material      = 0
  , bonusScores   = evaluatePositionBonuses   scoresBatch pos
  , penaltyScores = evaluatePositionPenalties             pos
  }
  where
    scoresBatch
      | isKingInCheck pos = emptyScoresBatch
      | otherwise         = getScoresBatch pos


evaluatePositionBonuses :: ScoresBatch -> Position -> BonusBreakdown
evaluatePositionBonuses scores pos =
  BonusBreakdown {
    mobility    = scores.mobility
  , kingThreats = scores.kingThreats
  , bishopPair  = evaluateBishopPair pos
  -- evalScore Passed pawns
  -- evalScore knight outposts
  }

evaluatePositionPenalties :: Position -> PenaltyBreakdown
evaluatePositionPenalties Position {player, pawns} =
  PenaltyBreakdown {
    isolatedPawns = evaluateIsolatedPawns (player & pawns)
  }


evaluateBishopPair :: Position -> Score
evaluateBishopPair Position {player, bishops} =
   bishopPairBonus * clamp (0, 1) (onesScore (player & bishops) - 1)


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


getScoresBatch :: Position -> ScoresBatch
getScoresBatch Position {..} = ScoresBatch {..}
  where
    !mobility        =
        knightsMobility + bishopsMobility + rooksMobility + queensMobility

    !kingThreats     =
      (kingThreatPiecesTable !! piecesCount) * kingThreatScore `div` 100

    !piecesCount     =
      knightsCount + bishopsCount + rooksCount + queensCount

    !kingThreatScore =
        byMinorThreatBonus * fromIntegral (byKnightThreats + byBishopThreats)
      + byRookThreatBonus  * fromIntegral byRookThreats
      + byQueenThreatBonus * fromIntegral byQueenThreats

    (knightsMobility, byKnightThreats, knightsCount) =
      foldBoardScores knightMobilityTable
        knightAttacks
        (unpinned&knights)

    (bishopsMobility, byBishopThreats, bishopsCount) =
      foldBoardScores bishopMobilityTable
        (bishopMoves allPieces pinnedPieces king)
        (player&bishops)

    (rooksMobility, byRookThreats, rooksCount)       =
      foldBoardScores rookMobilityTable
        (rookMoves allPieces pinnedPieces king)
        (player&rooks)

    (queensMobility, byQueenThreats, queensCount)    =
      foldBoardScores queenMobilityTable
        (queenMoves allPieces pinnedPieces king)
        (player&queens)


    foldBoardScores !mobilityTable !f !board =
      foldlBoard (0, 0, 0) foldFn f board
      where
        foldFn (!x, !y, !z) !attackArea =
          (x + mobilityTable !! ones (attackArea .\ player),
           y + threatenedSquares,
           z + min 1 threatenedSquares)
          where
            !threatenedSquares = ones (enemyKingArea & attackArea)

    !king          = player & kings
    !unpinned      = player .\ pinnedPieces
    !allPieces     = player .| enemy
    !enemyKingArea = kingAttacks (lsb (enemy&kings))


data ScoresBatch = ScoresBatch {
  mobility    :: Score,
  kingThreats :: Score
}

emptyScoresBatch :: ScoresBatch
emptyScoresBatch = ScoresBatch {
    mobility = 0
  , kingThreats = 0
}


class EvalScore a where
  evalScore :: a -> Score

data ScoreBreakdown = ScoreBreakdown
  { white :: PlayerScoreBreakdown
  , black :: PlayerScoreBreakdown
  }

data PlayerScoreBreakdown = PlayerScoreBreakdown
  { material      :: Score
  , bonusScores   :: BonusBreakdown
  , penaltyScores :: PenaltyBreakdown
  }


data BonusBreakdown = BonusBreakdown
  { mobility    :: Score
  , kingThreats :: Score
  , bishopPair  :: Score
  }

newtype PenaltyBreakdown = PenaltyBreakdown
  { isolatedPawns :: Score
  }

instance EvalScore ScoreBreakdown where
  evalScore ScoreBreakdown {..} =
    evalScore white - evalScore black


instance EvalScore PlayerScoreBreakdown where
  evalScore PlayerScoreBreakdown {..} =
    material + evalScore bonusScores - evalScore penaltyScores


instance EvalScore BonusBreakdown where
  evalScore BonusBreakdown {..} =
    mobility + kingThreats + bishopPair


instance EvalScore PenaltyBreakdown where
  evalScore PenaltyBreakdown {..} =
    isolatedPawns

instance Show ScoreBreakdown where
  show breakdown@ScoreBreakdown {..} = unlines
    ["White: "       <> indentBreak (show white),
     "Black: "       <> indentBreak (show black),
     totalScoreLine,
     "Total: "       <> show (evalScore breakdown),
     totalScoreLine]


instance Show PlayerScoreBreakdown where
  show breakdown@PlayerScoreBreakdown {..} = unlines
    ["Material: "       <> show material,
     "Bonus Scores: "   <> indentBreak (show bonusScores),
     "Penalty Scores: " <> indentBreak (show penaltyScores),
     totalScoreLine,
     "Player Total: "          <> show (evalScore breakdown),
     totalScoreLine]


instance Show BonusBreakdown where
  show breakdown@BonusBreakdown {..} = unlines
    ["Mobility: "     <> show mobility,
     "King Threats: " <> show kingThreats,
     "Bishop Pair: "  <> show bishopPair,
     totalScoreLine,
     "Bonus Total: "        <> show (evalScore breakdown),
     totalScoreLine]


instance Show PenaltyBreakdown where
  show breakdown@PenaltyBreakdown {..} = unlines
    ["Isolated pawns:" <> show isolatedPawns,
     totalScoreLine,
     "Penalty Total: "         <> show (evalScore breakdown),
     totalScoreLine]


indentBreak :: String -> String
indentBreak str = "\n" <> indent 2 str

totalScoreLine :: String
totalScoreLine = replicate 15 '-'


onesBoard :: Board -> Board
onesBoard !x = fromIntegral $! ones x

onesScore :: Board -> Score
onesScore !x = fromIntegral $! ones x
