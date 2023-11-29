{- HLINT ignore "Use camelCase" -}

module MoveGen.PieceBoards (knightMovesVec, kingMovesVec, fileMovesVec, rankMovesVec, diagMovesVec, antiDiagMovesVec, westMovesVec, northMovesVec, southMovesVec, eastMovesVec, northWestMovesVec, northEastMovesVec, southEastMovesVec, southWestMovesVec, longCastleSliding, shortCastleSliding, rank_1, rank_2, rank_3, rank_4, rank_5, rank_6, rank_7, rank_8, file_A, file_B, file_C, file_D, file_E, file_F, file_G, file_H) where

import           AppPrelude          hiding (fmap, map, (<$>))

import           Data.Vector.Unboxed as Vector
import           Models.Board
import           Utils.Ord           (inRange)


knightMove :: Square -> Board
knightMove n =
    move1 .| move2 .| move3 .| move4 .| move5
          .| move6 .| move7 .| move8
  where
    move1 = (board .\ (file_G .| file_A .| rank_8)) << 10
    move3 = (board .\ (file_H .| rank_7 .| rank_8)) << 17
    move4 = (board .\ (file_A .| rank_7 .| rank_8)) << 15
    move5 = (board .\ (file_B .| file_H .| rank_8)) << 6
    move6 = (board .\ (file_B .| file_H .| rank_1)) >> 10
    move7 = (board .\ (file_A .| rank_2 .| rank_1)) >> 17
    move8 = (board .\ (file_H .| rank_2 .| rank_1)) >> 15
    move2 = (board .\ (file_G .| file_A .| rank_1)) >> 6
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


fileMove :: Square -> Board
fileMove n = ranks !! (n / 8)

rankMove :: Square -> Board
rankMove n = files !! (n % 8)

diagMove :: Square -> Board
diagMove n = antiDiags !! (n % 8 + n / 8)

antiDiagMove :: Square -> Board
antiDiagMove n = diags !! (7 - n % 8 + n / 8 )

getRank :: Rank -> Board
getRank n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << (8 * n) <<)

getFile :: File -> Board
getFile n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << n <<) . (* 8)

getDiag :: Diag -> Board
getDiag = diagHelper (7 -)

getAntiDiag :: Int -> Board
getAntiDiag = diagHelper id

diagHelper :: (Int -> Int) -> Int -> Board
diagHelper f n = foldl1 (.|) xs
  where
  xs = Vector.fromList do
    x <- Vector.toList sideSquares
    let y = f (n - x) `rem` 8
    guard $! inRange 0 7 (n - x)
    pure $! toBoard (y + 8 * x)


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
ranks = getRank <$> sideSquares

files :: Vector Board
files = getFile <$> sideSquares

diags :: Vector Board
diags = getDiag <$> diagonals

antiDiags :: Vector Board
antiDiags = getAntiDiag <$> diagonals


-- Cached Piece moves
knightMovesVec :: Vector Board
knightMovesVec = knightMove <$> squares

kingMovesVec :: Vector Board
kingMovesVec = kingMove <$> squares


-- Sliding moves
fileMovesVec :: Vector Board
fileMovesVec = fileMove <$> squares

rankMovesVec :: Vector Board
rankMovesVec = rankMove <$> squares

diagMovesVec :: Vector Board
diagMovesVec = diagMove <$> squares

antiDiagMovesVec :: Vector Board
antiDiagMovesVec = antiDiagMove <$> squares

westMovesVec :: Vector Board
westMovesVec = westMove <$> squares

northMovesVec :: Vector Board
northMovesVec = northMove <$> squares

eastMovesVec :: Vector Board
eastMovesVec = eastMove <$> squares

southMovesVec :: Vector Board
southMovesVec = southMove <$> squares

northWestMovesVec :: Vector Board
northWestMovesVec = northWestMove <$> squares

northEastMovesVec :: Vector Board
northEastMovesVec = northEastMove <$> squares

southEastMovesVec :: Vector Board
southEastMovesVec = southEastMove <$> squares

southWestMovesVec :: Vector Board
southWestMovesVec = southWestMove <$> squares


-- Ranges
squares :: Vector Square
squares = Vector.fromList [0 .. 63]

sideSquares :: Vector SideSquare
sideSquares = Vector.fromList [0 .. 7]

diagonals :: Vector Diagonal
diagonals = Vector.fromList [0 .. 14]

longCastleSliding :: Board
longCastleSliding = file_C .| file_D

shortCastleSliding :: Board
shortCastleSliding = file_F .| file_G

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

(<$>) :: (Int -> Board) -> Vector Int -> Vector Board
(<$>) = map
