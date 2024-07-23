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

knightScore :: (?phase :: Phase) => Score
knightScore = taperScore $ ScorePair 340 320

rookScore :: (?phase :: Phase) => Score
rookScore = taperScore $ ScorePair 500 550

queenScore :: (?phase :: Phase) => Score
queenScore = taperScore $ ScorePair 1000 1050


-- Bonuses
bishopPairBonus :: (?phase :: Phase) => Score
bishopPairBonus = taperScore $ ScorePair 25 100

knightOutpostBonus :: (?phase :: Phase) => Score
knightOutpostBonus = taperScore $ ScorePair 50 0

rookOnSemiOpenFileBonus :: (?phase :: Phase) => Score
rookOnSemiOpenFileBonus = taperScore $ ScorePair 10 0

pawnShield1RankBonus :: (?phase :: Phase) => Score
pawnShield1RankBonus = taperScore $ ScorePair 20 0

pawnShield2RankBonus :: (?phase :: Phase) => Score
pawnShield2RankBonus = taperScore $ ScorePair 10 0

unstoppablePawnBonus :: Score
unstoppablePawnBonus = 700

kingEscortedPassedPawnBonus :: (?phase :: Phase) => Score
kingEscortedPassedPawnBonus = taperScore $ ScorePair 0 10

passedPawnTable :: Vector ScorePair
passedPawnTable = Vector.fromList $ map (uncurry ScorePair)
  [(0, 0), (0, 5), (5, 10), (10, 40),
   (15, 75), (25, 120), (35, 170)]

freePassedPawnTable :: Vector ScorePair
freePassedPawnTable = Vector.fromList $ map (ScorePair 0)
  [0, 10, 35, 80, 140, 210, 300]


knightMobilityTable :: Vector ScorePair
knightMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-15, -30), (-5, -10), (-1, -2), (2, 4), (5, 10),
   (8, 16), (11, 21), (13, 26), (15, 30)]

bishopMobilityTable :: Vector ScorePair
bishopMobilityTable = Vector.fromList $ map (uncurry ScorePair)
  [(-25, -75), (-11, -33), (-6, -18), (-1, -3),
   (3, 9), (6, 18), (9, 27), (12, 36), (14, 42),
   (17, 51), (19, 57), (21, 63), (23, 69), (25, 75)]

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

isolatedPawnPenalty :: Score
isolatedPawnPenalty = 25

doubledPawnPenalty :: Score
doubledPawnPenalty = 20

minorPieceThreat :: Score
minorPieceThreat = 30

rookThreat :: Score
rookThreat = 50

queenThreat :: Score
queenThreat = 80

pieceTradesPenalty :: Score
pieceTradesPenalty = 15

pawnTradesPenalty :: Score
pawnTradesPenalty = 30



kingThreatPiecesTable :: Vector Score
kingThreatPiecesTable = Vector.fromList
  [0, 0, 0, 50, 75, 88, 94, 97, 99, 100,
   100, 100, 100, 100, 100, 100, 100]


blackKnightSquareTable :: Vector ScorePair
blackKnightSquareTable = Vector.fromList $ map (`ScorePair` 0)
  [-20,-10,-10,-10,-10,-10,-10,-20,
   -10,-5,  0,  0,  0,  0, -5,-10,
   -10,  0, 10, 20, 20, 10,  0,-10,
   -10,  0, 20, 20, 20, 20,  0,-10,
   -10,  0, 20, 20, 20, 20,  0,-10,
   -10,  0, 10, 20, 20, 10,  0, -10,
   -10,-5,  0,  0,  0,  0, -5,-10,
   -20,-10,-10,-10,-10,-10,-10,-20]

blackBishopSquareTable :: Vector ScorePair
blackBishopSquareTable = Vector.fromList $ map (`ScorePair` 0)
  [-10,-10,-10,-10,-10,-10,-10,-10,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5, 10, 10,  5,  0,-10,
   -10,  0,  5, 10, 10,  5,  0,-10,
   -10,  0, 5, 10, 10, 5,  0,-10,
   -10,  5, 10, 10, 10, 10,  5,-10,
   -10,  5,  5,  5,  5,  5, 5,-10,
   -10,-10,-10,-10,-10,-10,-10,-10]

blackKingSquareTable :: Vector ScorePair
blackKingSquareTable = Vector.fromList $ map (`ScorePair` 0)
  [-30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -10,-20,-20,-30,-30,-20,-20,-10,
     10, 10, 0, -10, -10, 0,  10, 10,
    20, 20, 20,  0,  0, 0, 20, 20]

whiteKnightSquareTable :: Vector ScorePair
whiteKnightSquareTable = reverseSquareTable blackKnightSquareTable

whiteBishopSquareTable :: Vector ScorePair
whiteBishopSquareTable = reverseSquareTable blackBishopSquareTable

whiteKingSquareTable :: Vector ScorePair
whiteKingSquareTable = reverseSquareTable blackKingSquareTable

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
