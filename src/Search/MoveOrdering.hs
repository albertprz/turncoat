module Search.MoveOrdering where

import           AppPrelude

import           Evaluation.StaticExchange
import           Models.KillersTable       (KillersTable)
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove          (makeMove)
import           MoveGen.MoveQueries       (isKingInCheck, isLegalQuietMove)
import           MoveGen.PieceCaptures     (allCaptures)
import           MoveGen.PieceQuietMoves   (allQuietMoves)


-- Move Ordering:
-- - Transposition table (PV / Refutation) move
-- - Winning captures (SEE >= 0) (Ordered by SEE)
-- - 2 Killer moves
-- - Quiet moves (Ordered by Static Eval)
-- - Losing captures (SEE < 0) (Ordered by SEE)

-- Reduced moves:
-- Late killers / quiet moves

{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?killersTable :: KillersTable, ?tTable::TTable)
  => Depth -> Ply -> Position -> IO ([Move], [Move])
getSortedMoves !depth !ply pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- filter (`notElem` ttMove) <$> getSortedKillers ply pos
  let
    hashMoves = ttMove <> killerMoves
    allMoves = ttMove
      <> filter (`notElem` ttMove) winningCaptures
      <> killerMoves
      <> filter (`notElem` hashMoves) quietMoves
      <> filter (`notElem` ttMove) losingCaptures
  pure if notInCheck && depth >= 4
    then splitAt 6 allMoves
    else (allMoves, [])
  where
    notInCheck = not (isKingInCheck pos)
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = allQuietMoves     pos


{-# INLINE  getSortedKillers #-}
getSortedKillers :: (?killersTable::KillersTable)
  => Ply -> Position -> IO [Move]
getSortedKillers !ply pos
  = sortOn (Down . getMoveScore) <$> killerMoves
  where
    getMoveScore mv = -(makeMove mv pos).materialScore
    killerMoves     =
      filter (`isLegalQuietMove` pos) <$> KillersTable.lookupMoves ply


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos
  = bimapBoth (map fst)
      $ partition ((>= 0) . snd)
      $ sortOn (Down . snd)
      $ map attachEval captures
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)
    captures      = allCaptures pos
