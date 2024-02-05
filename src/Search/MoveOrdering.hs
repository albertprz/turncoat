module Search.MoveOrdering where

import           AppPrelude

import           Evaluation.Evaluation     (evaluatePosition)
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
  pure if notInCheck && depth >= 2
    then splitAt 4 allMoves
    else (allMoves, [])
  where
    notInCheck = not $ isKingInCheck pos
    (winningCaptures, losingCaptures) = getSortedCaptures   pos
    quietMoves                        = getSortedQuietMoves pos


{-# INLINE  getSortedKillers #-}
getSortedKillers :: (?killersTable::KillersTable)
  => Ply -> Position -> IO [Move]
getSortedKillers !ply pos =
  sortOn (Down . getMoveScore) <$> killerMoves
  where
    getMoveScore mv = - evaluatePosition (makeMove mv pos)
    killerMoves     =
      filter (`isLegalQuietMove` pos) <$> KillersTable.lookupMoves ply


{-# INLINE  getSortedCaptures #-}
getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimapBoth (map fst)
    $ partition ((>= 0) . snd)
    $ sortOn (Down . snd)
    $ map attachEval
    $ allCaptures pos
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)


{-# INLINE  getSortedQuietMoves #-}
getSortedQuietMoves :: Position -> [Move]
getSortedQuietMoves pos =
  sortOn (Down . getMoveScore) $ allQuietMoves pos
  where
    getMoveScore mv = - evaluatePosition (makeMove mv pos)


{-# INLINE  getWinningCaptures #-}
getWinningCaptures :: Position -> [Move]
getWinningCaptures pos =
  filter ((>= 0) . (`evaluateCaptureExchange` pos))
    $ allCaptures pos
