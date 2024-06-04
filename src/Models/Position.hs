module Models.Position where


import           AppPrelude
import           Bookhound.Utils.List      (hasMultiple)
import           Data.Bitraversable        (bisequence)
import           Data.List.Split           (chunksOf)
import           Data.Maybe                (fromJust)
import           Models.Move               (foldlBoard)
import           Models.Piece
import           Models.Score
import           Models.TranspositionTable (ZKey (ZKey))
import           Utils.Board


data Position = Position {
   previousPositions :: ~[ZKey]
  , materialScore    :: Score
  , halfMoveClock    :: Ply
  , color            :: Color
  , player           :: Board
  , enemy            :: Board
  , pawns            :: Board
  , knights          :: Board
  , bishops          :: Board
  , rooks            :: Board
  , queens           :: Board
  , kings            :: Board
  , enPassant        :: Board
  , castling         :: Board
  , attacked         :: Board
  , leapingCheckers  :: Board
  , sliderCheckers   :: Board
  , pinnedPieces     :: Board
}


startPosition :: Position
startPosition = emptyPosition {
    color    = White
  , player   = rank_1 .| rank_2
  , enemy    = rank_7 .| rank_8
  , pawns    = rank_2 .| rank_7
  , rooks    = (rank_1 .| rank_8) & (file_A .| file_H)
  , knights  = (rank_1 .| rank_8) & (file_B .| file_G)
  , bishops  = (rank_1 .| rank_8) & (file_C .| file_F)
  , queens   = (rank_1 .| rank_8) & file_D
  , kings    = (rank_1 .| rank_8) & file_E
  , castling = (rank_1 .| rank_8) & (file_A .| file_E .| file_H)
}


emptyPosition :: Position
emptyPosition = Position {
   previousPositions = []
  , color             = White
  , halfMoveClock     = 0
  , materialScore     = 0
  , player            = 0
  , enemy             = 0
  , pawns             = 0
  , rooks             = 0
  , knights           = 0
  , bishops           = 0
  , queens            = 0
  , kings             = 0
  , castling          = 0
  , attacked          = 0
  , enPassant         = 0
  , leapingCheckers   = 0
  , sliderCheckers    = 0
  , pinnedPieces      = 0

}


{-# INLINE  getZobristKey #-}
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
    piece  | testSquare pawns n   = Just Pawn
           | testSquare knights n = Just Knight
           | testSquare bishops n = Just Bishop
           | testSquare rooks n   = Just Rook
           | testSquare queens n  = Just Queen
           | testSquare kings n   = Just King
           | otherwise            = Nothing

    color' | testSquare player n = Just color
           | testSquare enemy n  = Just $ reverseColor color
           | otherwise           = Nothing


{-# INLINE  maybeCapturedPieceAt #-}
maybeCapturedPieceAt :: Square -> Position -> Maybe Piece
maybeCapturedPieceAt n (Position {..})
  | testSquare pawns n   = Just Pawn
  | testSquare knights n = Just Knight
  | testSquare bishops n = Just Bishop
  | testSquare rooks n   = Just Rook
  | testSquare queens n  = Just Queen
  | otherwise            = Nothing


{-# INLINE  isPieceAt #-}
isPieceAt :: Piece -> Square -> Position -> Bool
isPieceAt piece n Position {..} =
  case piece of
    Pawn   -> testSquare pawns n
    Knight -> testSquare knights n
    Bishop -> testSquare bishops n
    Rook   -> testSquare rooks n
    Queen  -> testSquare queens n
    King   -> testSquare kings n


{-# INLINE  isRepeatedPosition #-}
isRepeatedPosition :: ZKey -> Position -> Bool
isRepeatedPosition zKey Position {..} =
  hasMultiple $ filter (== zKey) previousPositions


instance Show Position where
  show pos =
    show pos.color <> " to play: \n\n" <> boardStr
    where
      boardStr = intercalate "\n" (intersperse ' ' <$> pieces)
      pieces = reverse $ chunksOf 8 do
        n <- toList squares
        pure $ maybe ' ' pieceToChar (pieceAt n pos)
