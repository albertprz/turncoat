module Evaluation.Constants where

import           AppPrelude
import           Utils.Board
import           Data.List.Split      (chunksOf)
import qualified Data.Vector.Storable as Vector
import           Models.Piece
import           Models.Score


bishopPairBonus :: Score
bishopPairBonus = 25

isolatedPawnPenalty :: Score
isolatedPawnPenalty = 25

minorSafetyPenalty :: Score
minorSafetyPenalty = 20

rookSafetyPenalty :: Score
rookSafetyPenalty = 40

queenSafetyPenalty :: Score
queenSafetyPenalty = 80


kingZonePenalty :: Vector Score
kingZonePenalty = Vector.fromList
  [0, 50, 75, 88, 94, 97, 99, 100, 100, 100]


knightMobilityTable :: Vector Score
knightMobilityTable = Vector.fromList
  [-25, -10, -2, 4, 9, 13, 19, 22, 25]

bishopMobilityTable :: Vector Score
bishopMobilityTable = Vector.fromList
  [-35, -18, -8, -1, 5, 10, 15, 19, 23, 26, 29, 31, 33, 35]

rookMobilityTable :: Vector Score
rookMobilityTable = Vector.fromList
  [-20, -12, -6, -1, 3, 6, 9, 11, 13, 15, 16, 17, 18, 19, 20]

queenMobilityTable :: Vector Score
queenMobilityTable = Vector.fromList
  [-30, -17, -10, -5, -2, 0, 2, 4, 6, 8, 10, 12, 14, 16,
   17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 ]


blackPawnSquareTable :: Vector Score
blackPawnSquareTable = Vector.fromList
  [0,  0,  0,  0,  0,  0,  0,  0,
   50, 50, 50, 50, 50, 50, 50, 50,
   10, 10, 20, 30, 30, 20, 10, 10,
   5,  5, 10, 25, 25, 10,  5,  5,
   0,  0,  0, 20, 20,  0,  0,  0,
   5, -5,-10,  0,  0,-10, -5,  5,
   5, 10, 10,-20,-20, 10, 10,  5,
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
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5, 10, 10,  5,  0,-10,
   -10,  5,  5, 10, 10,  5,  5,-10,
   -10,  0, 10, 10, 10, 10,  0,-10,
   -10, 10, 10, 10, 10, 10, 10,-10,
   -10,  5,  0,  0,  0,  0,  5,-10,
   -20,-10,-10,-10,-10,-10,-10,-20]

blackRookSquareTable :: Vector Score
blackRookSquareTable = Vector.fromList
  [0,  0,  0,  0,  0,  0,  0,  0,
   5, 10, 10, 10, 10, 10, 10,  5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
   0,  0,  0,  5,  5,  0,  0,  0]

blackQueenSquareTable :: Vector Score
blackQueenSquareTable = Vector.fromList
  [-20,-10,-10, -5, -5,-10,-10,-20,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5,  5,  5,  5,  0,-10,
    -5,  0,  5,  5,  5,  5,  0, -5,
     0,  0,  5,  5,  5,  5,  0, -5,
   -10,  5,  5,  5,  5,  5,  0,-10,
   -10,  0,  5,  0,  0,  0,  0,-10,
   -20,-10,-10, -5, -5,-10,-10,-20]

blackKingSquareTable :: Vector Score
blackKingSquareTable = Vector.fromList
  [-30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -10,-20,-20,-20,-20,-20,-20,-10,
    20, 20,  0,  0,  0,  0, 20, 20,
    20, 30, 10,  0,  0, 10, 30, 20]

whitePawnSquareTable :: Vector Score
whitePawnSquareTable =
  reverseSquareTable blackPawnSquareTable

whiteKnightSquareTable :: Vector Score
whiteKnightSquareTable =
  reverseSquareTable blackKnightSquareTable

whiteBishopSquareTable :: Vector Score
whiteBishopSquareTable =
  reverseSquareTable blackBishopSquareTable

whiteRookSquareTable :: Vector Score
whiteRookSquareTable =
  reverseSquareTable blackRookSquareTable

whiteQueenSquareTable :: Vector Score
whiteQueenSquareTable =
  reverseSquareTable blackQueenSquareTable

whiteKingSquareTable :: Vector Score
whiteKingSquareTable =
  reverseSquareTable blackKingSquareTable


pawnSquareTable :: Vector Score
pawnSquareTable =
  whitePawnSquareTable <> blackPawnSquareTable

knightSquareTable :: Vector Score
knightSquareTable =
  whiteKnightSquareTable <> blackKnightSquareTable

bishopSquareTable :: Vector Score
bishopSquareTable =
  whiteBishopSquareTable <> blackBishopSquareTable

rookSquareTable :: Vector Score
rookSquareTable =
  whiteRookSquareTable <> blackRookSquareTable

queenSquareTable :: Vector Score
queenSquareTable =
  whiteQueenSquareTable <> blackQueenSquareTable

kingSquareTable :: Vector Score
kingSquareTable =
  whiteKingSquareTable <> blackKingSquareTable


getSquareTableIndex :: Board -> Color -> Square
getSquareTableIndex board color =
  lsb board + 64 * fromIntegral color


reverseSquareTable :: Vector Score -> Vector Score
reverseSquareTable =
  fromList . fold . reverse . chunksOf 8 . toList
