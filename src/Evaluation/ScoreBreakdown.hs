module Evaluation.ScoreBreakdown where

import           AppPrelude

import           Bookhound.Utils.Text
import           Foreign.Storable.Generic
import           Models.Score


data ScoreBreakdown = ScoreBreakdown
  { playerScores :: PlayerScoreBreakdown
  , enemyScores  :: PlayerScoreBreakdown
  }

data PlayerScoreBreakdown = PlayerScoreBreakdown
  { material      :: MaterialBreakdown
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
  { mobility           :: Score
  , passedPawns        :: Score
  , bishopPair         :: Score
  , knightOutposts     :: Score
  , rooksOnOpenFile    :: Score
  , rooksOnSeventhRank :: Score
  }

data PenaltyBreakdown = PenaltyBreakdown
  { kingThreats   :: Score
  , isolatedPawns :: Score
  }

data ScorePair = ScorePair Score Score
  deriving Generic

instance GStorable ScorePair

class EvalScore a where
  evalScore :: a -> Score


instance EvalScore ScoreBreakdown where
  evalScore ScoreBreakdown {..} =
    evalScore playerScores - evalScore enemyScores


instance EvalScore PlayerScoreBreakdown where
  evalScore PlayerScoreBreakdown {..} =
      evalScore material
    + evalScore bonusScores
    - evalScore penaltyScores

instance EvalScore MaterialBreakdown where
  evalScore MaterialBreakdown {..} =
      evalScore queens  + evalScore rooks
    + evalScore bishops + evalScore knights
    + evalScore pawns   + evalScore kings


instance EvalScore BonusBreakdown where
  evalScore BonusBreakdown {..} =
      mobility        + passedPawns
    + bishopPair      + knightOutposts
    + rooksOnOpenFile + rooksOnSeventhRank


instance EvalScore PenaltyBreakdown where
  evalScore PenaltyBreakdown {..} =
    kingThreats + isolatedPawns

instance EvalScore ScorePair where
  evalScore (ScorePair x y) = x + y


instance Show ScoreBreakdown where
  show breakdown@ScoreBreakdown {..} = unlines
    ["Player: " <> indentBreak (show playerScores),
     "Enemy:  " <> indentBreak (show enemyScores),
     totalScoreLine,
     "Total: "  <> show (evalScore breakdown),
     totalScoreLine]



instance Show PlayerScoreBreakdown where
  show breakdown@PlayerScoreBreakdown {..} = unlines
    ["Material:       " <> indentBreak (show material),
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
    ["Mobility:              " <> show mobility,
     "Passed Pawns:          " <> show passedPawns,
     "Bishop Pair:           " <> show bishopPair,
     "Knight Outposts:       " <> show knightOutposts,
     "Rooks On Open Files:   " <> show rooksOnOpenFile,
     "Rooks On Seventh Rank: " <> show rooksOnSeventhRank,
     totalScoreLine,
     "Bonus Total: "           <> show (evalScore breakdown),
     totalScoreLine]


instance Show PenaltyBreakdown where
  show breakdown@PenaltyBreakdown {..} = unlines
    ["King Threats:   " <> show kingThreats,
     "Isolated pawns: " <> show isolatedPawns,
     totalScoreLine,
     "Penalty Total: "  <> show (evalScore breakdown),
     totalScoreLine]

instance Show ScorePair where
  show (ScorePair x y) = "(" <> fill 4 (show x) <> ", " <> show y <> ")"


fill :: Int -> String -> String
fill n str = replicate (n - length str) ' ' <> str

indentBreak :: String -> String
indentBreak str = "\n" <> indent 2 str

totalScoreLine :: String
totalScoreLine = replicate 20 '-'
