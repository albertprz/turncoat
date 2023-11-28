module Models.Board where

import           AppPrelude
import           Data.Bits
import           Data.Bits.Extras    (Ranked (lsb))
import           Data.List           (iterate)
import qualified Data.Vector.Unboxed as Vector
import           Models.Piece        (Piece)

type Board = Word64
type Move = (Piece, Square, Board)

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
  foldlBoard moves (flip cons) mapFn board
  where
    mapFn n = (piece, n, f n)

foldlBoard :: a -> (a -> b -> a) -> (Square -> b) -> Board -> a
foldlBoard = go 0
  where
    go _ acc _ _ board | board == 0 = acc
    go i acc foldFn mapFn board    = go i' acc' foldFn mapFn board'
      where
        acc' = foldFn acc (mapFn (i' - 1))
        i' = i + next
        board' = board >> next
        next = lsb board


showMove :: Move -> Text
showMove (piece, square, board) =
  tshow piece <> " at " <> showSquare square <> ":"
  <> "\n" <> showBoard board

showBoard :: Board -> Text
showBoard board = pack $ unlines $ map showBin
                       $ reverse $ take 8
                       $ iterate (>> 8) board
  where
    showBin w = intersperse ' ' [sb (testBit w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = 'X'

showSquare :: Square -> Text
showSquare n = pack [rankChars !! rank, fileChars !! file]
  where
    file = n % 8
    rank = n / 8

fileChars :: Vector Char
fileChars = Vector.fromList ['A' .. 'H']

rankChars :: Vector Char
rankChars = Vector.fromList ['1' .. '8']
