module Models.Move where

import           AppPrelude
import           Constants.Boards
import           Models.Piece

import           Data.Bits
import           Data.List           (iterate)
import qualified Data.Vector.Unboxed as Vector


data Move = Move {
  piece     :: Piece,
  promotion :: Maybe Promotion,
  start     :: Square,
  end       :: Square
} deriving (Eq, Ord, Generic)


instance Hashable Move


{-# INLINE  foldMapBoard #-}
foldMapBoard ::  (Square -> Board) -> Board -> Board
foldMapBoard = foldlBoard 0 (.|)

{-# INLINE  foldBoardMoves #-}
foldBoardMoves :: Piece -> (Square -> Board) -> Board -> [Move] -> [Move]
foldBoardMoves piece f board moves =
  foldlBoard moves (foldBoardSquares piece f) id board

{-# INLINE  foldBoardSquares #-}
foldBoardSquares :: Piece -> (Square -> Board) -> [Move] -> Square -> [Move]
foldBoardSquares Pawn f moves start =
  foldlBoard moves foldFn id (f start)
  where
    foldFn xs end
      | toBoard end & (rank_1 .| rank_8) /= 0 =
        Move Pawn (Just QueenProm) start end
        : Move Pawn (Just KnightProm) start end
        : Move Pawn (Just RookProm) start end
        : Move Pawn (Just BishopProm) start end
        : xs
      | otherwise =
        Move Pawn Nothing start end : xs

foldBoardSquares piece f moves start =
  foldlBoard moves (flip cons) mapFn (f start)
  where
    mapFn = Move piece Nothing start

{-# INLINE  foldlBoard #-}
foldlBoard :: a -> (a -> b -> a) -> (Square -> b) -> Board -> a
foldlBoard = go 0
  where
    go _ !acc _ _ !board | board == 0 = acc
    go !i !acc !foldFn !mapFn !board = go (i' + 1) acc' foldFn mapFn board'
      where
        !acc'    = foldFn acc $! mapFn i'
        !i'      = i + current
        !board'  = (board >> current) >> 1
        !current = lsb board

showBoard :: Board -> String
showBoard board = unlines $ map showBin
                          $ reverse $ take 8
                          $ iterate (>> 8) board
  where
    showBin w = intersperse ' ' [sb (testBit w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = 'X'

showSquare :: Square -> String
showSquare n = [fileChars !! file, rankChars !! rank]
  where
    file = n % 8
    rank = n / 8


fileChars :: Vector Char
fileChars = Vector.fromList ['A' .. 'H']

rankChars :: Vector Char
rankChars = Vector.fromList ['1' .. '8']

instance Show Move where
  show (Move {..}) =
    show piece <> " at "
    <> showSquare start <> " to "
    <> maybe "" ((<> " ") . show) promotion
    <> showSquare end
