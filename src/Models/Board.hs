module Models.Board where

import           AppPrelude
import           Data.Bits
import           Data.List  (iterate)


data Position = Position {
  player  :: Board,
  enemy   :: Board,
  pawns   :: Board,
  knights :: Board,
  bishops :: Board,
  rooks   :: Board,
  queens  :: Board,
  kings   :: Board
}

type Board = Word64
type Square = Int
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

infixl 9 <<|
(<<|) :: Shift
(<<|) = shiftL

infixl 9 |>>
(|>>) :: Shift
(|>>) = shiftR

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

position :: Int -> Board
position n = 1 << n

showBoard :: Word64 -> Text
showBoard b = pack $ unlines $ map showBin
                   $ reverse $ take 8
                   $ iterate (>> 8) b
  where
    showBin w = intersperse ' ' [sb (testBit w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = '1'
