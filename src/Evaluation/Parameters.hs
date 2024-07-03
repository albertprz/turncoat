module Evaluation.Parameters where

import           AppPrelude
import           Data.List.Split           (chunksOf)
import           Evaluation.ScoreBreakdown
import           Models.Piece
import           Models.Score
import           Utils.Board

import qualified Data.Vector.Storable      as Vector


-- Material scores
pawnScore :: Score
pawnScore = 100

bishopScore :: Score
bishopScore = 340

knightScore :: Score
knightScore = 340

rookScore :: (?phase :: Phase) => Score
rookScore = taperScore $ ScorePair 500 550

queenScore :: (?phase :: Phase) => Score
queenScore = taperScore $ ScorePair 1000 1080


-- Bonuses
bishopPairBonus :: (?phase :: Phase) => Score
bishopPairBonus = taperScore $ ScorePair 25 100

knightOutpostBonus :: (?phase :: Phase) => Score
knightOutpostBonus = taperScore $ ScorePair 30 0

rookOnSemiOpenFileBonus :: (?phase :: Phase) => Score
rookOnSemiOpenFileBonus = taperScore $ ScorePair 20 10

rookOnSeventhRankBonus :: Score
rookOnSeventhRankBonus = 10

freePasserBonus :: Score
freePasserBonus = 100

passedPawnTable :: Vector ScorePair
passedPawnTable = Vector.fromList $ map (uncurry ScorePair)
  [(0, 0), (0, 0), (10, 20), (20, 40),
   (30, 60), (40, 80), (50, 100)]

knightMobilityTable :: Vector ScorePair
knightMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-15, -30), (-5, -10), (-1, -2), (2, 4), (6, 12),
   (9, 18), (11, 22), (13, 26), (15, 30)]

bishopMobilityTable :: Vector ScorePair
bishopMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-25, -50), (-11, -22), (-6, -12), (-1, -2),
   (3, 6), (6, 12), (9, 18), (12, 24), (14, 28),
   (17, 34), (19, 38), (21, 42), (23, 46), (25, 50)]

rookMobilityTable :: Vector ScorePair
rookMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-10, -50), (-4, -20), (-2, -10), (0, 0), (1, 5),
   (2, 10), (3, 15), (4, 20), (5, 25), (6, 30),
   (7, 34), (8, 38), (9, 42), (10, 46), (10, 50)]

queenMobilityTable :: Vector ScorePair
queenMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-10, -50), (-6, -30), (-5, -22), (-4, -16), (-2, -10),
   (-1, -5), (0, -1), (1, 3), (1, 7), (2, 11), (2, 14),
   (3, 17), (3, 20), (4, 23), (4, 26), (5, 28), (5, 30),
   (6, 32), (6, 34), (7, 36), (7, 38), (8, 40), (8, 42),
   (9, 44), (9, 46), (10, 48), (10, 49), (10, 50)]


-- Penalties
threatByMinorPenalty :: Score
threatByMinorPenalty = 20

threatByRookPenalty :: Score
threatByRookPenalty = 40

threatByQueenPenalty :: Score
threatByQueenPenalty = 80

isolatedPawnPenalty :: (?phase :: Phase) => Score
isolatedPawnPenalty = taperScore $ ScorePair 25 50

kingThreatPiecesTable :: Vector Score
kingThreatPiecesTable = Vector.fromList
  [0, 0, 50, 75, 88, 94, 97, 99, 100,
   100, 100, 100, 100, 100, 100, 100]


-- Piece Square Tables
blackPawnSquareTable :: Vector Score
blackPawnSquareTable = Vector.fromList
  [0,  0,  0,  0,  0,  0,  0,  0,
  10, 10, 20, 30, 30, 20, 10, 10,
  10, 10, 10, 20, 20, 10, 10, 10,
   5,  5, 5, 15, 15, 5, 5,  5,
   0,  0,  0, 10, 10,  0,  0,  0,
  -5, -5,-10,  0,  0,-10, -5,  -5,
   10, 10, 10,-20,-20, 10, 10, 10,
   0,  0,  0,  0,  0,  0,  0,  0]

blackKnightSquareTable :: Vector Score
blackKnightSquareTable = Vector.fromList
  [-50,-40,-30,-30,-30,-30,-40,-50,
   -40,-20,  0,  0,  0,  0,-20,-40,
   -30,  0, 10, 15, 15, 10,  0,-30,
   -30,  5, 15, 20, 20, 15,  5,-30,
   -30,  0, 15, 20, 20, 15,  0,-30,
   -30,  5, 10, 15, 15, 10,  5,-30,
   -40,-20,  0,  5,  5,  0,-20,-40,
   -50,-40,-30,-30,-30,-30,-40,-50]

blackBishopSquareTable :: Vector Score
blackBishopSquareTable = Vector.fromList
  [-20,-10,-10,-10,-10,-10,-10,-20,
   -10,  10,  0,  0,  0,  0,  10,-10,
   -10,  10,  10, 10, 10,  10, 10,-10,
   -10,  10,  10, 10, 10,  10, 10,-10,
   -10,  10, 10, 10, 10,  10,  10,-10,
   -10, 10, 10, 10, 10, 10, 10,-10,
   -10, 10,  0,  0,  0,  0, 10,-10,
   -20,-10,-10,-10,-10,-10,-10,-20]

blackKingMidgameSquareTable :: Vector Score
blackKingMidgameSquareTable = Vector.fromList
  [-30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -10,-20,-20,-30,-30,-20,-20,-10,
   -10,-10,-10,-20,-20,-10,-10,-10,
    30, 30, 10,  0,  0, 10, 30, 30]

blackKingEndgameSquareTable :: Vector Score
blackKingEndgameSquareTable = Vector.fromList
  [-50,-30,-30,-20,-20,-30,-30,-50,
   -30,-20,-10,  10, 10,-10,-20,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 30, 40, 40, 30,-10,-30,
   -30,-10, 20, 30, 30, 20,-10,-30,
   -30,-20, -10, 10, 10, -10,-20,-30,
   -50,-30,-30,-20,-20,-30,-30,-50]


blackKingSquareTable :: Vector ScorePair
blackKingSquareTable =
  Vector.zipWith ScorePair blackKingMidgameSquareTable
                           blackKingEndgameSquareTable

whitePawnSquareTable :: Vector Score
whitePawnSquareTable =
  reverseSquareTable blackPawnSquareTable

whiteKnightSquareTable :: Vector Score
whiteKnightSquareTable =
  reverseSquareTable blackKnightSquareTable

whiteBishopSquareTable :: Vector Score
whiteBishopSquareTable =
  reverseSquareTable blackBishopSquareTable

whiteKingSquareTable :: Vector ScorePair
whiteKingSquareTable =
  reverseSquareTable blackKingSquareTable


infixl 9 !!%
(!!%) :: (?phase::Phase) => Vector ScorePair -> Int -> Score
(!!%) table idx = taperScore (table !! idx)


taperScore :: (?phase :: Phase) => ScorePair -> Score
taperScore (ScorePair mgScore egScore) =
  (?phase * mgScore + (totalPhase - ?phase) * egScore) / totalPhase


getSquareTableIndex :: Board -> Color -> Square
getSquareTableIndex board color =
  lsb board + 64 * fromIntegral color


reverseSquareTable :: Storable a => Vector a -> Vector a
reverseSquareTable =
  fromList . fold . reverse . chunksOf 8 . toList
