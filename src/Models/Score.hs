module Models.Score where

import           AppPrelude

import           Data.Int        (Int16)
import           Models.Piece
import           Test.QuickCheck (Arbitrary (..), elements)


type Score = Int16
type Phase = Int16
type Depth = Word8
type Ply   = Word8
type Age   = Word8


newtype NodeType = NodeType Word8
  deriving (Eq, Ord, Num, Storable, Show)

instance Arbitrary NodeType where
  arbitrary = elements [PV, Cut, All]


{-# COMPLETE PV, Cut, All #-}
pattern PV, Cut, All :: NodeType
pattern PV  = NodeType 0
pattern Cut = NodeType 1
pattern All = NodeType 2


getNodeType :: Score -> Score -> Score -> NodeType
getNodeType !alpha !beta !score
  | score >= beta  = Cut
  | score > alpha = PV
  | otherwise     = All


pieceToPhase :: Piece -> Phase
pieceToPhase = \case
  Knight -> minorPiecePhase
  Bishop -> minorPiecePhase
  Rook   -> rookPhase
  Queen  -> queenPhase
  _      -> 0


minorPiecePhase :: Phase
minorPiecePhase = 1

rookPhase :: Phase
rookPhase = 2

queenPhase :: Phase
queenPhase = 4

totalPhase :: Phase
totalPhase =
  minorPiecePhase * 8 + rookPhase * 4 + queenPhase * 2
