module Models.Move where

import           AppPrelude
import           Constants.Boards
import           Models.Piece

import           Data.Bits
import           Data.List                (iterate)
import qualified Data.Vector.Storable     as Vector
import           Foreign.Storable.Generic


data Move = Move {
  piece     :: Piece,
  promotion :: Maybe Promotion,
  start     :: Square,
  end       :: Square
} deriving (Eq, Ord, Generic)

newtype StorableMove = StorableMove Int
  deriving (Eq, Ord, Num, Generic, Storable)

instance GStorable StorableMove

instance Hashable Move


{-# INLINE  encodeMove #-}
encodeMove :: Maybe Move -> StorableMove
encodeMove Nothing = 1
encodeMove (Just Move {..}) = StorableMove
  (fromIntegral pieceN << 1
  .|. fromIntegral promotionN << 8
  .|. start << 16
  .|. end << 24)
  where
    Piece pieceN = piece
    Promotion promotionN = fromMaybe (Promotion 0) promotion

{-# INLINE  decodeMove #-}
decodeMove :: StorableMove -> Maybe Move
decodeMove (StorableMove n)
  | testBit n 0 = Nothing
  | otherwise = Just Move {
      piece = piece,
      promotion = if promotion /= Promotion 0 then Just promotion else Nothing,
      start = start,
      end = end
    }
    where
      piece = Piece $ fromIntegral n
      promotion = Promotion $ fromIntegral (n >> 8)
      start = start >> 16
      end = end >> 24


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
showSquare n = [fileChars !! toFile n, rankChars !! toRank n]
  where
    fileChars = Vector.fromList ['a' .. 'h']
    rankChars = Vector.fromList ['1' .. '8']


instance Show Move where
  show (Move {..}) =
    showSquare start
    <> showSquare end
    <> foldMap show promotion
