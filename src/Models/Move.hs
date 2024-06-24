module Models.Move (Move(..), StorableMove(..), encodeMove, decodeMove,  foldBoardAttacks, foldBoardMoves, foldBoardMovesConst, foldBoardPawnMovesConst, foldBoardSquares, foldlBoard, showBoard) where

import           AppPrelude
import           Models.Piece
import           Utils.Board

import           Data.Bits
import           Data.List            (iterate)
import qualified Data.Vector.Storable as Vector
import           Test.QuickCheck      (Arbitrary (..), chooseInt)


data Move = Move {
  piece     :: Piece,
  promotion :: Promotion,
  start     :: Square,
  end       :: Square
} deriving (Eq, Ord, Generic)

instance Hashable Move

instance Arbitrary Move where
  arbitrary = Move
    <$> arbitrary <*> arbitrary
    <*> chooseInt (0, 63) <*> chooseInt (0, 63)

newtype StorableMove = StorableMove Word32
  deriving (Eq, Ord, Num, Bits, Generic, Storable)


encodeMove :: Maybe Move -> StorableMove
encodeMove Nothing = bit 31
encodeMove (Just Move {..}) = StorableMove
  (fromIntegral start
  .|. (fromIntegral end << 8)
  .|. (fromIntegral pieceN << 16)
  .|. (fromIntegral promotionN << 24))
  where
    Piece pieceN = piece
    Promotion promotionN = promotion

decodeMove :: StorableMove -> Maybe Move
decodeMove (StorableMove n)
  | testBit n 31 = Nothing
  | otherwise = Just $! Move {
      start = start,
      end = end,
      piece = piece,
      promotion = promotion
    }
    where
      start = fromIntegral n .&. 63
      end = fromIntegral (n >> 8) .&. 63
      piece = Piece $ fromIntegral (n >> 16)
      promotion = Promotion $ fromIntegral ((n >> 24) .&. 7)


{-# INLINE  foldBoardAttacks #-}
foldBoardAttacks :: (Square -> Board) -> Board -> Board
foldBoardAttacks !f !board = foldlBoard 0 (.|) f board


{-# INLINE  foldBoardMoves #-}
foldBoardMoves :: Piece -> (Square -> Board) -> Board -> [Move] -> [Move]
foldBoardMoves !piece !f !board moves =
  foldlBoard moves (foldBoardSquares piece f) id board


{-# INLINE  foldBoardMovesConst #-}
foldBoardMovesConst :: Piece -> Square -> Board -> [Move] -> [Move]
foldBoardMovesConst !piece !end !board moves =
  foldlBoard moves genMoves id board
  where
    genMoves xs start = Move piece NoProm start end : xs


{-# INLINE  foldBoardPawnMovesConst #-}
foldBoardPawnMovesConst :: Square -> Board -> [Move] -> [Move]
foldBoardPawnMovesConst !end !board moves =
  foldlBoard moves genMoves id board
  where
    genMoves xs start = Move Pawn promotion start end : xs
    !promotion
      | testSquare (rank_1 .| rank_8) end = QueenProm
      | otherwise                         = NoProm


{-# INLINE  foldBoardSquares #-}
foldBoardSquares :: Piece -> (Square -> Board) -> [Move] -> Square -> [Move]
foldBoardSquares Pawn !f moves !start =
    foldlBoard moves (flip $ genPawnMoves start) id $! f start

foldBoardSquares piece !f moves !start =
    foldlBoard moves (flip cons) (Move piece NoProm start) (f start)


{-# INLINE  genPawnMoves #-}
genPawnMoves :: Square -> Square -> [Move] -> [Move]
genPawnMoves !start !end xs
  | testSquare (rank_1 .| rank_8) end =
    Move Pawn QueenProm start end
    : Move Pawn KnightProm start end
    : Move Pawn RookProm start end
    : Move Pawn BishopProm start end
    : xs
  | otherwise =
    Move Pawn NoProm start end : xs


{-# INLINE  foldlBoard #-}
foldlBoard :: a -> (a -> b -> a) -> (Square -> b) -> Board -> a
foldlBoard !initial !foldFn !mapFn = go 0 initial
  where
    go  _ acc  0     = acc
    go !i !acc board = go (i' + 1) acc' board'
      where
        acc'    = foldFn acc $! mapFn i'
        i'      = i + current
        board'  = (board >> current) >> 1
        current = lsb board


showBoard :: Board -> String
showBoard board = unlines $ map showBin
                          $ reverse $ take 8
                          $ iterate (>> 8) board
  where
    showBin w = intersperse ' ' [sb (testSquare w i) | i <- [0 .. 7]]
    sb False = '0'
    sb True  = 'X'


showSquare :: Square -> String
showSquare n = [fileChars !! toFile n, rankChars !! toRank n]
  where
    fileChars = Vector.fromList ['a' .. 'h']
    rankChars = Vector.fromList ['1' .. '8']


instance Show Move where
  show (Move {..}) = showSquare start <> showSquare end <> show promotion
