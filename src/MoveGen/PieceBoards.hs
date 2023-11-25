{- HLINT ignore "" -}

module MoveGen.PieceBoards (knightMoves, kingMoves, fileMoves, rankMoves, diagMoves, antiDiagMoves, westMoves, northMoves, southMoves, eastMoves, northWestMoves, northEastMoves, southEastMoves, southWestMoves, rank_1, rank_2, rank_3, rank_4, rank_5, rank_6, rank_7, rank_8, file_A, file_B, file_C, file_D, file_E, file_F, file_G, file_H) where

import           AppPrelude

import           Data.List    (foldl1, (!!))
import           Models.Board
import           Utils.Ord    (inRange)


knightMove :: Int -> Board
knightMove n =
    move1 .| move2 .| move3 .| move4 .| move5
          .| move6 .| move7 .| move8
  where
    pos = 1 << n
    move1 = (pos .\ (file_B .| file_A .| rank_8)) << 10
    move2 = (pos .\ (file_B .| file_A .| rank_1)) >> 6
    move3 = (pos .\ (file_A .| rank_7 .| rank_8)) << 17
    move4 = (pos .\ (file_H .| rank_7 .| rank_8)) << 15
    move5 = (pos .\ (file_G .| file_H .| rank_8)) << 6
    move6 = (pos .\ (file_G .| file_H .| rank_1)) >> 10
    move7 = (pos .\ (file_H .| rank_2 .| rank_1)) >> 17
    move8 = (pos .\ (file_A .| rank_2 .| rank_1)) >> 15

kingMove :: Int -> Board
kingMove n =
    move1 .| move2 .| move3 .| move4 .| move5
          .| move6 .| move7 .| move8
  where
    pos = 1 << n
    move1 = (pos .\ file_A) << 1
    move2 = (pos .\ (file_A .| rank_8)) << 9
    move3 = (pos .\ file_H) >> 1
    move4 = (pos .\ (file_H .| rank_1)) >> 9
    move5 = (pos .\ rank_8) << 8
    move6 = (pos .\ (file_H .| rank_8)) << 7
    move7 = (pos .\ rank_1) >> 8
    move8 = (pos .\ (file_A .| rank_1)) >> 7


fileMove :: Square -> Board
fileMove n = files !! (n % 8)

rankMove :: Square -> Board
rankMove n = ranks !! (n / 8)

diagMove :: Int -> Board
diagMove n = diags !! (n % 8 + n / 8 )

antiDiagMove :: Int -> Board
antiDiagMove n = antiDiags !! (7 - n % 8 + n / 8 )

getRank :: Rank -> Board
getRank n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << n << 3 <<)

getFile :: File -> Board
getFile n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << n <<) . (<< 3)

getDiag :: Int -> Board
getDiag = diagHelper (7 -)

getAntiDiag :: Int -> Board
getAntiDiag = diagHelper id

diagHelper :: (Int -> Int) -> Int -> Board
diagHelper f n = foldl1 (.|) xs
  where
  xs = do
    x <- sideSquares
    let y = f (n - x) `rem` 8
    guard $ inRange 0 7 (n - x)
    pure $ 1 << (y + 8 * x)


-- Move Functions
westMove :: Square -> Board
westMove n = aboveMask n & fileMoves !! n

northMove :: Square -> Board
northMove n = aboveMask n & rankMoves !! n

eastMove :: Square -> Board
eastMove n = belowMask n & fileMoves !! n

southMove :: Square -> Board
southMove n = belowMask n & rankMoves !! n

northWestMove :: Square -> Board
northWestMove n = aboveMask n & antiDiagMoves !! n

northEastMove :: Square -> Board
northEastMove n = aboveMask n & diagMoves !! n

southEastMove :: Square -> Board
southEastMove n = belowMask n & antiDiagMoves !! n

southWestMove :: Square -> Board
southWestMove n = belowMask n & diagMoves !! n


-- Masks
aboveMask :: Square -> Board
aboveMask n = ((1 << 63) - (1 << n)) << 1

belowMask :: Square -> Board
belowMask n = ((1 << n) - 1)


-- Directions
ranks :: [Board]
ranks = getRank <$> sideSquares

files :: [Board]
files = getFile <$> sideSquares

diags :: [Board]
diags = getDiag <$> diagonals

antiDiags :: [Board]
antiDiags = getAntiDiag <$> diagonals


-- Cached Piece moves
knightMoves :: [Board]
knightMoves = knightMove <$> squares

kingMoves :: [Board]
kingMoves = kingMove <$> squares


-- Sliding moves
fileMoves :: [Board]
fileMoves = fileMove <$> squares

rankMoves :: [Board]
rankMoves = rankMove <$> squares

diagMoves :: [Board]
diagMoves = diagMove <$> squares

antiDiagMoves :: [Board]
antiDiagMoves = antiDiagMove <$> squares

westMoves :: [Board]
westMoves = westMove <$> squares

northMoves :: [Board]
northMoves = northMove <$> squares

eastMoves :: [Board]
eastMoves = eastMove <$> squares

southMoves :: [Board]
southMoves = southMove <$> squares

northWestMoves :: [Board]
northWestMoves = northWestMove <$> squares

northEastMoves :: [Board]
northEastMoves = northEastMove <$> squares

southEastMoves :: [Board]
southEastMoves = southEastMove <$> squares

southWestMoves :: [Board]
southWestMoves = southWestMove <$> squares


-- Ranges
squares :: [Int]
squares = [0 .. 63]

sideSquares :: [Int]
sideSquares = [0 .. 7]

diagonals :: [Int]
diagonals = [0 .. 14]


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
