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

-- Reduced moves (LMR):
-- Late quiet moves and losing captures, except for checks

{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?killersTable :: KillersTable, ?tTable::TTable)
  => Depth -> Ply -> Position -> IO ([Move], [Move])
getSortedMoves !depth !ply pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- filter (andPred (`notElem` ttMove)
                                (`isLegalQuietMove` pos))
                       <$> KillersTable.lookupMoves ply
  let
    allMoves = ttMove
      <> filter (`notElem` ttMove) winningCaptures
      <> killerMoves
      <> filter (`notElem` (ttMove <> killerMoves)) quietMoves
      <> filter (`notElem` ttMove)                  losingCaptures

  pure if depth >= 2 && not (isKingInCheck pos)
    then splitAt 4 allMoves
    else (allMoves, [])
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves pos


{-# INLINE  getSortedFutilityMoves #-}
getSortedFutilityMoves :: (?tTable::TTable) => Score -> Position -> IO [Move]
getSortedFutilityMoves !threshold pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  pure (ttMove
    <> filter (`notElem` ttMove) captures
    <> filter (`notElem` ttMove) checks)
  where
    captures = fst $ getSortedCapturesGreaterThan threshold pos
    checks   = filter (isKingInCheck . (`makeMove` pos))
                      (getSortedQuietMoves pos)


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures = getSortedCapturesGreaterThan 0


{-# INLINE  getSortedCapturesGreaterThan #-}
getSortedCapturesGreaterThan :: Score -> Position -> ([Move], [Move])
getSortedCapturesGreaterThan !n pos =
  bimapBoth (map fst)
  $ partition ((>= n) . snd)
  $ sortOn (Down . snd)
  $ map attachEval captures
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)
    captures = allCaptures pos


{-# INLINE  getSortedQuietMoves #-}
getSortedQuietMoves :: Position -> [Move]
getSortedQuietMoves pos =
  sortOn (Down . getMoveScore) quietMoves
  where
    getMoveScore mv = -(makeMove mv pos).materialScore
    quietMoves      = allQuietMoves pos
