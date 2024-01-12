module Evaluation.PieceSquareTables where

import           AppPrelude
import           Constants.Boards
import           Data.List.Split      (chunksOf)
import qualified Data.Vector.Storable as Vector
import           Models.Piece
import           Models.Score


{-# INLINE  getSquareTableIndex #-}
getSquareTableIndex :: Board -> Color -> Square
getSquareTableIndex board color =
  lsb board + 64 * fromIntegral color


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


reverseSquareTable :: Vector Score -> Vector Score
reverseSquareTable =
  fromList . fold . reverse . chunksOf 8 . toList
