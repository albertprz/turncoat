module Models.Board where

import           ClassyPrelude
import           Data.Bits


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

(&) :: Board -> Board -> Board
(&) = (.&.)

(#) :: Board -> Board -> Board
(#) = (.|.)

(^) :: Board -> Board -> Board
(^) = xor

(~) :: Board -> Board
(~) = complement

(<<) :: Board -> Int -> Board
(<<) = unsafeShiftL

(>>) :: Board -> Int -> Board
(>>) = unsafeShiftR

ones :: Board -> Int
ones = popCount

trailZeros :: Board -> Int
trailZeros = countTrailingZeros

leadZeros :: Board -> Int
leadZeros = countLeadingZeros
