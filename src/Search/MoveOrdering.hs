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
  => Depth -> Ply -> Position -> IO ([Move], [Move], [Move], [Move])
getSortedMoves !depth !ply pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- filter (andPred (`isLegalQuietMove` pos)
                                (`notElem` ttMove))
                  <$> KillersTable.lookupMoves ply
  let
    hashMoves = ttMove <> killerMoves
    allMoves = ttMove
      <> filter (`notElem` ttMove) winningCaptures
      <> killerMoves
      <> filter (`notElem` hashMoves) quietMoves
      <> filter (`notElem` ttMove) losingCaptures

  pure if | depth >= 6 && notInCheck -> splitAt3 4 6 6 allMoves
          | depth >= 4 && notInCheck -> splitAt2 4 6   allMoves
          | depth >= 2 && notInCheck -> splitAt1 4     allMoves
          | otherwise                -> (allMoves, [], [], [])
  where
    notInCheck = not (isKingInCheck pos)
    (winningCaptures, losingCaptures) = getSortedCaptures depth pos
    quietMoves                        = getSortedQuietMoves depth pos


{-# INLINE  getSortedFutilityMoves #-}
getSortedFutilityMoves :: (?tTable::TTable) => Score -> Position -> IO [Move]
getSortedFutilityMoves !threshold pos = do
  ttMove      <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  pure (ttMove <> captures <> checks)
  where
    captures = fst $ getSortedCapturesGreaterThan threshold 0 pos
    checks   = filter (isKingInCheck . (`makeMove` pos))
                      (allQuietMoves pos)


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Depth -> Position -> ([Move], [Move])
getSortedCaptures = getSortedCapturesGreaterThan 0


{-# INLINE  getSortedCapturesGreaterThan #-}
getSortedCapturesGreaterThan :: Score -> Depth -> Position -> ([Move], [Move])
getSortedCapturesGreaterThan !threshold !depth pos
  | depth >= 2 = bimapBoth (map fst)
      $ partition ((>= threshold) . snd)
      $ sortOn (Down . snd)
      $ map attachEval captures
  | otherwise = partition ((>= threshold) . (`evaluateCaptureExchange` pos))
                          captures
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)
    captures = allCaptures pos


{-# INLINE  getSortedQuietMoves #-}
getSortedQuietMoves :: Depth -> Position -> [Move]
getSortedQuietMoves !depth pos
  | depth >= 2 = sortOn (Down . getMoveScore) quietMoves
  | otherwise = quietMoves
  where
    getMoveScore mv = -(makeMove mv pos).materialScore
    quietMoves      = allQuietMoves pos
