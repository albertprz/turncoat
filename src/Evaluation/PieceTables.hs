module Evaluation.PieceTables where

import           AppPrelude
import           Constants.Boards
import           Data.List.Split      (chunksOf)
import qualified Data.Vector.Storable as Vector
import           Models.Piece
import           Models.Score


knightMobilityTable :: Vector Score
knightMobilityTable = Vector.fromList
  [-25, -10, -2, 4, 9, 13, 19, 22, 25]

bishopMobilityTable :: Vector Score
bishopMobilityTable = Vector.fromList
  [-35, -18, -8, -1, 5, 10, 15, 19, 23, 26, 29, 31, 33, 35]

rookMobilityTable :: Vector Score
rookMobilityTable = Vector.fromList
  [-12, -5, -2, 0, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]

queenMobilityTable :: Vector Score
queenMobilityTable = Vector.fromList
  [-10, -7, -4, -2, -1, 0, 1, 2, 2, 3, 3, 4, 4, 5,
   5, 6, 6, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 10 ]


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


{-# INLINE  getSquareTableIndex #-}
getSquareTableIndex :: Board -> Color -> Square
getSquareTableIndex board color =
  lsb board + 64 * fromIntegral color


reverseSquareTable :: Vector Score -> Vector Score
reverseSquareTable =
  fromList . fold . reverse . chunksOf 8 . toList
