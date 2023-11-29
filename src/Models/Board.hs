module Models.Board where

import           AppPrelude
import           Data.Bits
import           Data.Bits.Extras    (Ranked (lsb))
import           Data.List           (iterate)
import qualified Data.Vector.Unboxed as Vector
import           Models.Piece        (Piece)

type Board = Word64
type Move = (Piece, Square, Square)

type Square = Int
type SideSquare = Int
type Diagonal = Int
type Rank = Int
type File = Int
type Diag = Int
type AntiDiag = Int

type Shift = forall a. Bits a => a -> Int -> a


infixl 9 <<
(<<) :: Shift
(<<) = unsafeShiftL

infixl 9 >>
(>>) :: Shift
(>>) = unsafeShiftR

infixl 7 /
(/) :: Square -> Square -> Square
(/) = div

infixl 7 %
(%) :: Square -> Square -> Square
(%) = rem

infixl 8 &
(&) :: Board -> Board -> Board
(&) = (.&.)

infixl 8 .\
(.\) :: Board -> Board -> Board
(.\) x y = x & (!) y

infixl 7 .|
(.|) :: Board -> Board -> Board
(.|) = (.|.)

infixl 7 ^
(^) :: Board -> Board -> Board
(^) = xor

(!) :: Board -> Board
(!) = complement

ones :: Board -> Int
ones = popCount

trailZeros :: Board -> Int
trailZeros = countTrailingZeros

leadZeros :: Board -> Int
leadZeros = countLeadingZeros

toBoard :: Int -> Board
toBoard n = 1 << n


foldMapBoard ::  (Square -> Board) -> Board -> Board
foldMapBoard = foldlBoard 0 (.|)

foldBoardMoves :: Piece -> (Square -> Board) -> Board -> [Move] -> [Move]
foldBoardMoves piece f board moves =
  foldlBoard moves (foldBoardSquares piece f) id board

foldBoardSquares :: Piece -> (Square -> Board) -> [Move] -> Square -> [Move]
foldBoardSquares piece f moves start =
  foldlBoard moves (flip cons) mapFn (f start)
  where
    mapFn end = (piece, start, end)

foldlBoard :: a -> (a -> b -> a) -> (Square -> b) -> Board -> a
foldlBoard = go 0
  where
    go _ acc _ _ board | board == 0 = acc
    go i acc foldFn mapFn board    = go (i' + 1) acc' foldFn mapFn board'
      where
        acc'    = foldFn acc $! mapFn i'
        i'      = i + current
        board'  = board >> (current + 1)
        current = lsb board


showMove :: Move -> Text
showMove (piece, start, end) =
  tshow piece <> " at " <> showSquare start <> " -> "
  <> showSquare end

showBoard :: Board -> Text
showBoard board = pack $ unlines $ map showBin
                       $ reverse $ take 8
                       $ iterate (>> 8) board
  where
    showBin w = intersperse ' ' [sb (testBit w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = 'X'

showSquare :: Square -> Text
showSquare n = pack [fileChars !! file, rankChars !! rank]
  where
    file = n % 8
    rank = n / 8

fileChars :: Vector Char
fileChars = Vector.fromList ['A' .. 'H']

rankChars :: Vector Char
rankChars = Vector.fromList ['1' .. '8']
