module Evaluation.ScoreBreakdown where

import           AppPrelude

import           Bookhound.Utils.Text (indent)
import           Models.Score


data ScoreBreakdown = ScoreBreakdown
  { white :: PlayerScoreBreakdown
  , black :: PlayerScoreBreakdown
  }

data PlayerScoreBreakdown = PlayerScoreBreakdown
  { material      :: Maybe MaterialBreakdown
  , bonusScores   :: BonusBreakdown
  , penaltyScores :: PenaltyBreakdown
  }

data MaterialBreakdown = MaterialBreakdown
  { queens  :: ScorePair
  , rooks   :: ScorePair
  , bishops :: ScorePair
  , knights :: ScorePair
  , pawns   :: ScorePair
  , kings   :: ScorePair
  }

data BonusBreakdown = BonusBreakdown
  { mobility       :: Score
  , bishopPair     :: Score
  , knightOutposts :: Score
  , passedPawns    :: Score
  }

data PenaltyBreakdown = PenaltyBreakdown
  { kingThreats   :: Score
  , isolatedPawns :: Score
  }

data ScorePair = ScorePair Score Score

class EvalScore a where
  evalScore :: a -> Score


instance EvalScore ScoreBreakdown where
  evalScore ScoreBreakdown {..} =
    evalScore white - evalScore black


instance EvalScore PlayerScoreBreakdown where
  evalScore PlayerScoreBreakdown {..} =
    maybe 0 evalScore material
    + evalScore bonusScores
    - evalScore penaltyScores

instance EvalScore MaterialBreakdown where
  evalScore MaterialBreakdown {..} =
      evalScore queens  + evalScore rooks
    + evalScore bishops + evalScore knights
    + evalScore pawns   + evalScore kings


instance EvalScore BonusBreakdown where
  evalScore BonusBreakdown {..} =
      mobility       + bishopPair
    + knightOutposts + passedPawns


instance EvalScore PenaltyBreakdown where
  evalScore PenaltyBreakdown {..} =
    kingThreats + isolatedPawns

instance EvalScore ScorePair where
  evalScore (ScorePair x y) = x + y


instance Show ScoreBreakdown where
  show breakdown@ScoreBreakdown {..} = unlines
    ["White: "       <> indentBreak (show white),
     "Black: "       <> indentBreak (show black),
     totalScoreLine,
     "Total: "       <> show (evalScore breakdown),
     totalScoreLine]


instance Show PlayerScoreBreakdown where
  show breakdown@PlayerScoreBreakdown {..} = unlines
    ["Material:       " <> indentBreak (foldMap show material),
     "Bonus Scores:   " <> indentBreak (show bonusScores),
     "Penalty Scores: " <> indentBreak (show penaltyScores),
     totalScoreLine,
     "Player Total: "   <> show (evalScore breakdown),
     totalScoreLine]

instance Show MaterialBreakdown where
  show breakdown@MaterialBreakdown {..} = unlines
    ["Queens:  "        <> show queens,
     "Rooks:   "        <> show rooks,
     "Bishops: "        <> show bishops,
     "Knights: "        <> show knights,
     "Pawns:   "        <> show pawns,
     "Kings:   "        <> show kings,
     totalScoreLine,
     "Material Total: " <> show (evalScore breakdown),
     totalScoreLine]


instance Show BonusBreakdown where
  show breakdown@BonusBreakdown {..} = unlines
    ["Mobility:        " <> show mobility,
     "Bishop Pair:     " <> show bishopPair,
     "Knight Outposts: " <> show knightOutposts,
     "Passed Pawns:    " <> show passedPawns,
     totalScoreLine,
     "Bonus Total: " <> show (evalScore breakdown),
     totalScoreLine]


instance Show PenaltyBreakdown where
  show breakdown@PenaltyBreakdown {..} = unlines
    ["King Threats:   " <> show kingThreats,
     "Isolated pawns: " <> show isolatedPawns,
     totalScoreLine,
     "Penalty Total: " <> show (evalScore breakdown),
     totalScoreLine]

instance Show ScorePair where
  show (ScorePair x y) = "(" <> fill 4 (show x) <> ", " <> show y <> ")"


fill :: Int -> String -> String
fill n str = replicate (n - length str) ' ' <> str

indentBreak :: String -> String
indentBreak str = "\n" <> indent 2 str

totalScoreLine :: String
totalScoreLine = replicate 20 '-'
