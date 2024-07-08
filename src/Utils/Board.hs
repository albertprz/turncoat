{- HLINT ignore "Use camelCase" -}

module Utils.Board (Board, Square, (>>), (<<), (&), (.\), (.|), (^), (~),  popCount, popCountToBoard, lsb, msb, toBoard, toCondition, getSquareDistance, toReverseCondition, testSquare, toFile, toRank, knightMovesVec, kingMovesVec, fileMovesVec, rankMovesVec, diagMovesVec, antiDiagMovesVec, northEastMovesVec, northWestMovesVec, southEastMovesVec, southWestMovesVec, northMovesVec, westMovesVec, southMovesVec, eastMovesVec, shortCastleSlidingFiles, longCastleSlidingFiles, shortCastleFiles, longCastleFiles, whiteKnightOutpostRanks, blackKnightOutpostRanks, knightOupostFiles, castlingRngVec, enPassantRngVec, sideToMoveRng, pieceRngVec, whiteKnightOutpostAttackersVec, blackKnightOutpostAttackersVec, whitePassedPawnBlockersVec, blackPassedPawnBlockersVec, squares, rank_1, rank_2, rank_3, rank_4, rank_5, rank_6, rank_7, rank_8, file_A, file_B, file_C, file_D, file_E, file_F, file_G, file_H)  where

import           AppPrelude           hiding (foldl', map)

import qualified Data.Bits            as Bits
import           Data.Vector.Storable (foldl', foldl1, foldl1', map, slice)
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Random

type Board = Word64

type Square = Int
type SideSquare = Int
type Diagonal = Int
type Rank = Int
type File = Int
type Diag = Int


{-# SPECIALIZE (<<) :: Board -> Square -> Board #-}
infixl 9 <<
(<<) :: Bits.Bits a => a -> Square -> a
(<<) !x !y = Bits.unsafeShiftL x y

{-# SPECIALIZE (>>) :: Board -> Square -> Board #-}
infixl 9 >>
(>>) :: Bits.Bits a => a -> Square -> a
(>>) !x !y = Bits.unsafeShiftR x y

infixl 8 &
(&) :: Board -> Board -> Board
(&) !x !y = (Bits..&.) x y

infixl 7 .\
(.\) :: Board -> Board -> Board
(.\) !x !y = x & (~) y

infixl 7 .|
(.|) :: Board -> Board -> Board
(.|) !x !y = (Bits..|.) x y

infixl 7 ^
(^) :: Board -> Board -> Board
(^) !x !y = Bits.xor x y

infixl 9 ~
(~) :: Board -> Board
(~) !x = Bits.complement x

popCount :: Board -> Int
popCount !x = Bits.popCount x

popCountToBoard :: Board -> Board
popCountToBoard !x = fromIntegral $! popCount x

lsb :: Board -> Square
lsb !x = Bits.countTrailingZeros x

msb :: Board -> Square
msb !x = 65 * (zeros / 64) + 63 - zeros
  where
    !zeros = Bits.countLeadingZeros x

{-# SPECIALIZE toReverseCondition :: Board -> Board #-}
{-# SPECIALIZE toReverseCondition :: Square -> Square #-}
toReverseCondition :: (Num a, Ord a) => a -> a
toReverseCondition !x = 1 - toCondition x

{-# SPECIALIZE toCondition :: Board -> Board #-}
{-# SPECIALIZE toCondition :: Square -> Square #-}
toCondition :: (Ord a, Num a) => a -> a
toCondition !x = min 1 x

toBoard :: Square -> Board
toBoard !n = fromIntegral (1 - n / 64) * (1 << n)

testSquare :: Board -> Square -> Bool
testSquare !x !n = x & toBoard n /= 0

toFile :: Square -> File
toFile !n = n % 8

toRank :: Square -> Rank
toRank !n = n / 8

knightMove :: Square -> Board
knightMove n =
    move1 .| move2 .| move3 .| move4 .| move5
          .| move6 .| move7 .| move8
  where
    move1 = (board .\ (file_H .| file_G .| rank_8)) << 10
    move3 = (board .\ (file_H .| rank_7 .| rank_8)) << 17
    move4 = (board .\ (file_A .| rank_7 .| rank_8)) << 15
    move5 = (board .\ (file_A .| file_B .| rank_8)) << 6
    move6 = (board .\ (file_A .| file_B .| rank_1)) >> 10
    move7 = (board .\ (file_A .| rank_2 .| rank_1)) >> 17
    move8 = (board .\ (file_H .| rank_2 .| rank_1)) >> 15
    move2 = (board .\ (file_H .| file_G .| rank_1)) >> 6
    board = toBoard n

kingMove :: Square -> Board
kingMove n =
    move1 .| move2 .| move3 .| move4 .| move5
          .| move6 .| move7 .| move8
  where
    move1 = (board .\ file_H) << 1
    move2 = (board .\ (file_H .| rank_8)) << 9
    move3 = (board .\ file_A) >> 1
    move4 = (board .\ (file_A .| rank_1)) >> 9
    move5 = (board .\ rank_8) << 8
    move6 = (board .\ (file_A .| rank_8)) << 7
    move7 = (board .\ rank_1) >> 8
    move8 = (board .\ (file_H .| rank_1)) >> 7
    board = toBoard n


knightOutpostAttackers :: Bool -> Int -> Board
knightOutpostAttackers above n = filesBoard & ranksBoard
  where
    filesBoard = getPreviousFile file .| getNextFile file
    ranksBoard = foldl' (.|) 0
      $ map getRank
      $ filter (compareFn rank) sideSquares
    rank = toRank n
    file = toFile n
    compareFn | above     = (<)
              | otherwise = (>)

passedPawnBlockers :: Bool -> Int -> Board
passedPawnBlockers above n = filesBoard & ranksBoard
  where
    filesBoard = getPreviousFile file .| getFile file .| getNextFile file
    ranksBoard = foldl' (.|) 0
      $ map getRank
      $ filter (compareFn rank) sideSquares
    rank = toRank n
    file = toFile n
    compareFn | above     = (<)
              | otherwise = (>)


squareDistance :: Square -> Square -> Int
squareDistance n1 n2 = max fileDistance rankDistance
  where
    fileDistance = abs (file1 - file2)
    rankDistance = abs (rank1 - rank2)
    file1        = toFile n1
    file2        = toFile n2
    rank1        = toRank n1
    rank2        = toRank n2


fileMove :: Square -> Board
fileMove n = ranks !! toRank n

rankMove :: Square -> Board
rankMove n = files !! toFile n

diagMove :: Square -> Board
diagMove n = antiDiags !! (toFile n + toRank n)

antiDiagMove :: Square -> Board
antiDiagMove n = diags !! (7 - toFile n + toRank n)

getRank :: Rank -> Board
getRank n = foldl1' (.|) (map f sideSquares)
  where
    f = (1 << (8 * n) <<)

getFile :: File -> Board
getFile n = foldl1' (.|) (map f sideSquares)
  where
    f = (1 << n <<) . (* 8)

getPreviousFile :: File -> Board
getPreviousFile n
  | n == 0 = 0
  | otherwise = getFile (n - 1)

getNextFile :: File -> Board
getNextFile n
  | n == 7 = 0
  | otherwise = getFile (n + 1)

getSquareDistance :: Square -> Square -> Int
getSquareDistance n1 n2 =
  squareDistanceVec !! (n1 * 64 + n2)


getDiag :: Diag -> Board
getDiag = diagHelper (7 -)

getAntiDiag :: Int -> Board
getAntiDiag = diagHelper id

diagHelper :: (Int -> Int) -> Int -> Board
diagHelper f n = foldl1 (.|) xs
  where
  xs = fromList do
    x <- toList sideSquares
    let y = f (n - x) `rem` 8
    guard $ inRange 0 7 (n - x)
    pure $ toBoard (y + 8 * x)


-- Move Functions
northMove :: Square -> Board
northMove n = aboveMask n & rankMovesVec !! n

eastMove :: Square -> Board
eastMove n = aboveMask n & fileMovesVec !! n

westMove :: Square -> Board
westMove n = belowMask n & fileMovesVec !! n

southMove :: Square -> Board
southMove n = belowMask n & rankMovesVec !! n

northWestMove :: Square -> Board
northWestMove n = aboveMask n & diagMovesVec !! n

northEastMove :: Square -> Board
northEastMove n = aboveMask n & antiDiagMovesVec !! n

southEastMove :: Square -> Board
southEastMove n = belowMask n & diagMovesVec !! n

southWestMove :: Square -> Board
southWestMove n = belowMask n & antiDiagMovesVec !! n


-- Masks
aboveMask :: Square -> Board
aboveMask n = (toBoard 63 - toBoard n) << 1

belowMask :: Square -> Board
belowMask n = toBoard n - 1


-- Directions
ranks :: Vector Board
ranks = map getRank sideSquares

files :: Vector Board
files = map getFile sideSquares

diags :: Vector Board
diags = map getDiag diagonals

antiDiags :: Vector Board
antiDiags = map getAntiDiag diagonals


-- Cached Piece moves
knightMovesVec :: Vector Board
knightMovesVec = map knightMove squares

kingMovesVec :: Vector Board
kingMovesVec = map kingMove squares


-- Sliding moves
fileMovesVec :: Vector Board
fileMovesVec = snoc (map fileMove squares) 0

rankMovesVec :: Vector Board
rankMovesVec = snoc (map rankMove squares) 0

diagMovesVec :: Vector Board
diagMovesVec = snoc (map diagMove squares) 0

antiDiagMovesVec :: Vector Board
antiDiagMovesVec = snoc (map antiDiagMove squares) 0

westMovesVec :: Vector Board
westMovesVec = snoc (map westMove squares) 0

northMovesVec :: Vector Board
northMovesVec = snoc (map northMove squares) 0

eastMovesVec :: Vector Board
eastMovesVec = snoc (map eastMove squares) 0

southMovesVec :: Vector Board
southMovesVec = snoc (map southMove squares) 0

northWestMovesVec :: Vector Board
northWestMovesVec = snoc (map northWestMove squares) 0

northEastMovesVec :: Vector Board
northEastMovesVec = snoc (map northEastMove squares) 0

southEastMovesVec :: Vector Board
southEastMovesVec = snoc (map southEastMove squares) 0

southWestMovesVec :: Vector Board
southWestMovesVec = snoc (map southWestMove squares) 0


-- Evaluation
whiteKnightOutpostAttackersVec :: Vector Board
whiteKnightOutpostAttackersVec =
  map (knightOutpostAttackers True) squares

blackKnightOutpostAttackersVec :: Vector Board
blackKnightOutpostAttackersVec =
  map (knightOutpostAttackers False) squares

whitePassedPawnBlockersVec :: Vector Board
whitePassedPawnBlockersVec =
  map (passedPawnBlockers True) squares

blackPassedPawnBlockersVec :: Vector Board
blackPassedPawnBlockersVec =
  map (passedPawnBlockers False) squares

squareDistanceVec :: Vector Int
squareDistanceVec = fromList do
  n1 <- toList squares
  n2 <- toList squares
  pure $ squareDistance n1 n2


{-# NOINLINE pieceRngVec #-}
pieceRngVec :: Vector Board
pieceRngVec = unsafePerformIO
  (slice 0 768 <$> genRandoms)


{-# NOINLINE castlingRngVec #-}
castlingRngVec :: Vector Board
castlingRngVec = unsafePerformIO
  (slice 768 16 <$> genRandoms)


{-# NOINLINE enPassantRngVec #-}
enPassantRngVec :: Vector Board
enPassantRngVec = unsafePerformIO
  (slice 784 8 <$> genRandoms)


{-# NOINLINE sideToMoveRng #-}
sideToMoveRng :: Board
sideToMoveRng = unsafePerformIO
  ((!! 792) <$> genRandoms)


genRandoms :: IO (Vector Board)
genRandoms = fromList
  . take 1000 . randoms
  <$> getStdGen


-- Ranges
squares :: Vector Square
squares = fromList [0 .. 63]

sideSquares :: Vector SideSquare
sideSquares = fromList [0 .. 7]

diagonals :: Vector Diagonal
diagonals = fromList [0 .. 14]


-- Useful boards
shortCastleSlidingFiles :: Board
shortCastleSlidingFiles = file_F .| file_G

longCastleSlidingFiles :: Board
longCastleSlidingFiles = file_C .| file_D


shortCastleFiles :: Board
shortCastleFiles = file_F .| file_G .| file_H

longCastleFiles :: Board
longCastleFiles = file_A .| file_B .| file_C

whiteKnightOutpostRanks :: Board
whiteKnightOutpostRanks = rank_5 .| rank_6 .| rank_7

blackKnightOutpostRanks :: Board
blackKnightOutpostRanks = rank_4 .| rank_3 .| rank_2

knightOupostFiles :: Board
knightOupostFiles = file_C .| file_D .| file_E .| file_F


-- Ranks
rank_1 :: Board
rank_1 = getRank 0

rank_2 :: Board
rank_2 = getRank 1

rank_3 :: Board
rank_3 = getRank 2

rank_4 :: Board
rank_4 = getRank 3

rank_5 :: Board
rank_5 = getRank 4

rank_6 :: Board
rank_6 = getRank 5

rank_7 :: Board
rank_7 = getRank 6

rank_8 :: Board
rank_8 = getRank 7


-- Files
file_A :: Board
file_A = getFile 0

file_B :: Board
file_B = getFile 1

file_C :: Board
file_C = getFile 2

file_D :: Board
file_D = getFile 3

file_E :: Board
file_E = getFile 4

file_F :: Board
file_F = getFile 5

file_G :: Board
file_G = getFile 6

file_H :: Board
file_H = getFile 7
