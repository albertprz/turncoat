module Models.Board where

import           AppPrelude
import           Data.Bits
import           Data.List  (iterate)

type Board = Word64
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

foldMapBoard ::  (Board -> Board -> Board) -> (Square -> Board) -> Board -> Board
foldMapBoard = go 0 0
  where
    go i acc foldFn mapFn board
      | board == 0 = acc
      | otherwise =
        let
          acc' = foldFn acc $ mapFn (i' - 1)
          i' = i + zerosLen
          board' = board >> zerosLen
          zerosLen = trailZeros board + 1
        in go i' acc' foldFn mapFn board'


showBoard :: Word64 -> Text
showBoard b = pack $ unlines $ map showBin
                   $ reverse $ take 8
                   $ iterate (>> 8) b
  where
    showBin w = intersperse ' ' [sb (testBit w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = '1'
