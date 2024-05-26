module Models.Move  where

import           AppPrelude
import           Constants.Boards
import           Models.Piece

import           Data.Bits
import           Data.List                (iterate)
import qualified Data.Vector.Storable     as Vector
import           Foreign.Storable.Generic
import           Test.QuickCheck          (Arbitrary (..), chooseInt)


data Move = Move {
  piece     :: Piece,
  promotion :: Maybe Promotion,
  start     :: Square,
  end       :: Square
} deriving (Eq, Ord, Generic)

instance Arbitrary Move where
  arbitrary = Move
    <$> arbitrary <*> arbitrary
    <*> chooseInt (0, 63) <*> chooseInt (0, 63)

newtype StorableMove = StorableMove Word32
  deriving (Eq, Ord, Num, Bits, Generic, Storable)

instance GStorable StorableMove

instance Hashable Move


encodeMove :: Maybe Move -> StorableMove
encodeMove Nothing = bit 31
encodeMove (Just Move {..}) = StorableMove
  (fromIntegral start
  .|. (fromIntegral end << 8)
  .|. (fromIntegral pieceN << 16)
  .|. (fromIntegral promotionN << 24)
  )
  where
    Piece pieceN = piece
    Promotion promotionN = fromMaybe (Promotion 0) promotion

decodeMove :: StorableMove -> Maybe Move
decodeMove (StorableMove n)
  | testBit n 31 = Nothing
  | otherwise = Just Move {
      start = start,
      end = end,
      piece = piece,
      promotion = if promotion /= Promotion 0
        then Just promotion
        else Nothing
    }
    where
      start = fromIntegral n .&. 63
      end = fromIntegral (n >> 8) .&. 63
      piece = Piece $ fromIntegral (n >> 16)
      promotion = Promotion $ fromIntegral ((n >> 24) .&. 7)


{-# INLINE  foldBoard #-}
foldBoard ::  (Square -> Board) -> Board -> Board
foldBoard = foldlBoard 0 (.|)


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
      | testBit (rank_1 .| rank_8) end =
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
    go  _ !acc  _       _      0     = acc
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
showSquare n = [fileChars !! toFile n, rankChars !! toRank n]
  where
    fileChars = Vector.fromList ['a' .. 'h']
    rankChars = Vector.fromList ['1' .. '8']


instance Show Move where
  show (Move {..}) =
    showSquare start
    <> showSquare end
    <> foldMap show promotion
