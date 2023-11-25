{- HLINT ignore "" -}

module MoveGen.PieceBoards where

import           AppPrelude

import           Data.List    (foldl1)
import           Models.Board
import           Utils.Ord    (inRange)

rank :: Int -> Board
rank n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << n << 3 <<)

file :: Int -> Board
file n = foldl1 (.|) (map f sideSquares)
  where
    f = (1 << n <<) . (<< 3)

diag :: Int -> Board
diag = diagHelper (7 -)

antiDiag :: Int -> Board
antiDiag = diagHelper id

diagHelper :: (Int -> Int) -> Int -> Board
diagHelper f n = foldl1 (.|) xs
  where
  xs = do
    x <- sideSquares
    let y = f (n - x) `rem` 8
    guard $ inRange 0 7 (n - x)
    pure $ 1 << (y + 8 * x)

ranks :: [Board]
ranks = rank <$> sideSquares

files :: [Board]
files = file <$> sideSquares

diags :: [Board]
diags = diag <$> diagonals

antiDiags :: [Board]
antiDiags = antiDiag <$> diagonals

sideSquares :: [Int]
sideSquares = [0 .. 7]

diagonals :: [Int]
diagonals = [0 .. 15]



rank_1 :: Board
rank_1 = rank 0

rank_2 :: Board
rank_2 = rank 1

rank_3 :: Board
rank_3 = rank 2

rank_4 :: Board
rank_4 = rank 3

rank_5 :: Board
rank_5 = rank 4

rank_6 :: Board
rank_6 = rank 5

rank_7 :: Board
rank_7 = rank 6

rank_8 :: Board
rank_8 = rank 7


file_A :: Board
file_A = file 0

file_B :: Board
file_B = file 1

file_C :: Board
file_C = file 2

file_D :: Board
file_D = file 3

file_E :: Board
file_E = file 4

file_F :: Board
file_F = file 5

file_G :: Board
file_G = file 6

file_H :: Board
file_H = file 7
