module Search.MoveOrdering (getSortedMoves ) where

import           AppPrelude

import           Evaluation.Evaluation
import           Models.KillersTable       (KillersTable)
import qualified Models.KillersTable       as KillersTable
import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove          (makeMove)
import           MoveGen.MoveQueries
import           MoveGen.PieceCaptures     (allCaptures)
import           MoveGen.PieceQuietMoves   (allQuietMoves)
import           MoveGen.PositionQueries



-- Move Ordering:
-- - Transposition table (PV / Refutation) move
-- - Winning captures (SEE >= 0) (Ordered by SEE)
-- - 2 Killer moves
-- - Quiet moves (Ordered by Static Eval)
-- - Losing captures (SEE < 0) (Ordered by SEE)

-- Reduced moves:
-- Late killers / quiet moves

getSortedMoves :: (?killersTable :: KillersTable, ?tTable :: TTable)
  => Depth -> Ply -> Position -> IO (([Move], [Move]), Bool)
getSortedMoves !depth !ply pos = do
  ttMove   <- toList <$> TTable.lookupBestMove (getZobristKey pos)
  killerMoves <- filter (`notElem` ttMove) <$> getSortedKillers ply pos
  let
    hashMoves = ttMove <> killerMoves
    allTheMoves = ttMove
      <> filter (`notElem` ttMove) winningCaptures
      <> killerMoves
      <> filter (`notElem` hashMoves) quietMoves
      <> filter (`notElem` ttMove) losingCaptures
  pure $! if not (isKingInCheck pos) && depth >= 3
    then (splitAt 4 allTheMoves, notNull ttMove)
    else ((allTheMoves, []), notNull ttMove)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves depth pos
    notNull = not . null


getSortedKillers :: (?killersTable :: KillersTable)
  => Ply -> Position -> IO [Move]
getSortedKillers !ply pos = sortMoves pos <$> killerMoves
  where
    killerMoves =
      filter (`isLegalQuietMove` pos) <$> KillersTable.lookupMoves ply


getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimap mapFn mapFn
    $ partition ((>= 0) . snd)
    $ map attachEval
    $ allCaptures pos
  where
    !mapFn        = map fst . sortBy (comparing (Down . snd))
    attachEval mv = (mv, evaluateCaptureExchange mv pos)


getSortedQuietMoves :: Depth -> Position -> [Move]
getSortedQuietMoves depth pos
  | depth <= 2 = allQuietMoves pos
  | otherwise = sortMoves pos $ allQuietMoves pos


sortMoves :: Position -> [Move] -> [Move]
sortMoves pos = sortOn (Down . getMoveScore)
  where
    getMoveScore mv = - evaluatePosition (makeMove mv pos)
