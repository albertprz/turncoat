module Models.Position where


import           AppPrelude
import           Constants.Boards
import           Data.Bitraversable        (bisequence)
import           Data.Bits                 (Bits (..))
import           Data.List.Split           (chunksOf)
import           Data.Maybe                (fromJust)
import           Models.Move               (foldlBoard)
import           Models.Piece
import           Models.Score
import           Models.TranspositionTable (ZKey (ZKey))


data Position = Position {
    materialScore     :: Score
  , halfMoveClock     :: Ply
  , previousPositions :: [ZKey]
  , color             :: Color
  , attacked          :: Board
  , castling          :: Board
  , enPassant         :: Board
  , leapingCheckers   :: Board
  , sliderCheckers    :: Board
  , pinnedPieces      :: Board
  , player            :: Board
  , enemy             :: Board
  , pawns             :: Board
  , knights           :: Board
  , bishops           :: Board
  , rooks             :: Board
  , queens            :: Board
  , kings             :: Board
}


startPosition :: Position
startPosition = Position {
    materialScore = 0
  , color = White
  , halfMoveClock = 0
  , previousPositions = []
  , castling = (rank_1 .| rank_8) & (file_A .| file_E .| file_H)
  , attacked = 0
  , enPassant = 0
  , leapingCheckers = 0
  , sliderCheckers = 0
  , pinnedPieces = 0
  , player = rank_1 .| rank_2
  , enemy = rank_7 .| rank_8
  , pawns = rank_2 .| rank_7
  , rooks = (rank_1 .| rank_8) & (file_A .| file_H)
  , knights = (rank_1 .| rank_8) & (file_B .| file_G)
  , bishops = (rank_1 .| rank_8) & (file_C .| file_F)
  , queens = (rank_1 .| rank_8) & file_D
  , kings = (rank_1 .| rank_8) & file_E
}

emptyPosition :: Position
emptyPosition = Position {
    materialScore = 0
  , color = White
  , halfMoveClock = 0
  , previousPositions = []
  , castling = 0
  , attacked = 0
  , enPassant = 0
  , leapingCheckers = 0
  , sliderCheckers = 0
  , pinnedPieces = 0
  , player = 0
  , enemy = 0
  , pawns = 0
  , rooks = 0
  , knights = 0
  , bishops = 0
  , queens = 0
  , kings = 0
}

getZobristKey :: Position -> ZKey
getZobristKey pos@Position {..} = ZKey
  (piecesHash ^ castlingHash ^ enPassantHash ^ sideToMoveHash)
  where

    !piecesHash =
      foldlBoard 0 (^) getPieceHash (player .| enemy)

    !castlingHash = castlingRngVec !! idx
      where
      idx =   inBoard file_A + 2 * inBoard file_H
        + 4 * inBoard rank_1 + 8 * inBoard rank_8
      inBoard x = fromIntegral $ min 1 (castling & x)

    !enPassantHash = min 1 enPassant * enPassantRngVec !! idx
      where
        idx = toFile (lsb enPassant)

    !sideToMoveHash = fromIntegral colorN * sideToMoveRng
      where
        Color colorN = color

    getPieceHash n = pieceRngVec !! idx
      where
      idx = n + 64 * (fromIntegral piece + 6 * fromIntegral pieceColor)
      (Piece piece, Color pieceColor) = fromJust $ pieceAt n pos


{-# INLINE  pieceAt #-}
pieceAt :: Square -> Position -> Maybe (Piece, Color)
pieceAt n (Position {..}) = bisequence (piece, color')
  where
    piece  | testBit pawns n = Just Pawn
           | testBit knights n = Just Knight
           | testBit bishops n = Just Bishop
           | testBit rooks n = Just Rook
           | testBit queens n = Just Queen
           | testBit kings n = Just King
           | otherwise = Nothing

    color' | testBit player n = Just color
           | testBit enemy n = Just $ reverseColor color
           | otherwise = Nothing

{-# INLINE  maybeCapturedPieceAt #-}
maybeCapturedPieceAt :: Square -> Position -> Maybe Piece
maybeCapturedPieceAt n (Position {..})
  | testBit pawns n = Just Pawn
  | testBit knights n = Just Knight
  | testBit bishops n = Just Bishop
  | testBit rooks n = Just Rook
  | testBit queens n = Just Queen
  | otherwise = Nothing


{-# INLINE  isPieceAt #-}
isPieceAt :: Piece -> Square -> Position -> Bool
isPieceAt piece n Position {..} =
  case piece of
    Pawn   -> testBit pawns n
    Knight -> testBit knights n
    Bishop -> testBit bishops n
    Rook   -> testBit rooks n
    Queen  -> testBit queens n
    King   -> testBit kings n

{-# INLINE  isRepeatedPosition #-}
isRepeatedPosition :: ZKey -> Position -> Bool
isRepeatedPosition zKey Position {..} =
  length samePositions >= 2
  where
    samePositions = filter (== zKey) previousPositions

instance Show Position where
  show pos =
    show pos.color <> " to play: \n\n" <> boardStr
    where
      boardStr = intercalate "\n" (intersperse ' ' <$> pieces)
      pieces = reverse $ chunksOf 8 do
        n <- toList squares
        pure $ maybe ' ' pieceToChar (pieceAt n pos)
