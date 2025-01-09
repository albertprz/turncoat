module Evaluation.ScoreBreakdown where

import           AppPrelude

import           Models.Score

import           Bookhound.Utils.Text
import           Foreign.Storable.Generic


data ScoreBreakdown = ScoreBreakdown
  { playerBreakdown     :: PlayerScoreBreakdown
  , enemyBreakdown      :: PlayerScoreBreakdown
  , materialTradesScore :: Score
  }

data PlayerScoreBreakdown = PlayerScoreBreakdown
  { materialBreakdown :: MaterialBreakdown
  , bonusBreakdown    :: BonusBreakdown
  , penaltyBreakdown  :: PenaltyBreakdown
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
  { mobility        :: Score
  , passedPawns     :: Score
  , bishopPair      :: Score
  , knightOutposts  :: Score
  , rooksOnOpenFile :: Score
  , kingPawnShield  :: Score
  , castlingRights  :: Score
  }

data PenaltyBreakdown = PenaltyBreakdown
  { threats       :: Score
  , kingThreats   :: Score
  , isolatedPawns :: Score
  , doubledPawns  :: Score
  }

data ScorePair = ScorePair Score Score
  deriving Generic

instance GStorable ScorePair

class EvalScore a where
  evalScore :: a -> Score


instance EvalScore ScoreBreakdown where
  evalScore ScoreBreakdown {..} =
    evalScore playerBreakdown - evalScore enemyBreakdown


instance EvalScore PlayerScoreBreakdown where
  evalScore PlayerScoreBreakdown {..} =
      evalScore materialBreakdown
    + evalScore bonusBreakdown
    - evalScore penaltyBreakdown

instance EvalScore MaterialBreakdown where
  evalScore MaterialBreakdown {..} =
      evalScore queens  + evalScore rooks
    + evalScore bishops + evalScore knights
    + evalScore pawns   + evalScore kings


instance EvalScore BonusBreakdown where
  evalScore BonusBreakdown {..} =
      mobility        + passedPawns
    + bishopPair      + knightOutposts
    + rooksOnOpenFile + kingPawnShield
    + castlingRights


instance EvalScore PenaltyBreakdown where
  evalScore PenaltyBreakdown {..} =
      threats       + kingThreats
    + isolatedPawns + doubledPawns

instance EvalScore ScorePair where
  evalScore (ScorePair x y) = x + y


instance Show ScoreBreakdown where
  show breakdown@ScoreBreakdown {..} = unlines
    ["Player: " <> indentBreak (show playerBreakdown),
     "Enemy:  " <> indentBreak (show enemyBreakdown),
     totalScoreLine,
     "Total: "  <> show (evalScore breakdown),
     totalScoreLine]



instance Show PlayerScoreBreakdown where
  show breakdown@PlayerScoreBreakdown {..} = unlines
    ["Material:       " <> indentBreak (show materialBreakdown),
     "Bonus Scores:   " <> indentBreak (show bonusBreakdown),
     "Penalty Scores: " <> indentBreak (show penaltyBreakdown),
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
    ["Mobility:            " <> show mobility,
     "Passed Pawns:        " <> show passedPawns,
     "Bishop Pair:         " <> show bishopPair,
     "Knight Outposts:     " <> show knightOutposts,
     "Rooks On Open Files: " <> show rooksOnOpenFile,
     "King Pawn Shield:    " <> show kingPawnShield,
     "Castling Rights:     " <> show castlingRights,
     totalScoreLine,
     "Bonus Total: "         <> show (evalScore breakdown),
     totalScoreLine]


instance Show PenaltyBreakdown where
  show breakdown@PenaltyBreakdown {..} = unlines
    ["Threats:        " <> show threats,
     "King Threats:   " <> show kingThreats,
     "Isolated pawns: " <> show isolatedPawns,
     "Doubled pawns:  " <> show doubledPawns,
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
