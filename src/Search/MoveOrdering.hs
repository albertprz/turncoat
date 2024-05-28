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
    then (splitAt 5 allTheMoves, hasSome ttMove)
    else ((allTheMoves, []), hasSome ttMove)
  where
    (winningCaptures, losingCaptures) = getSortedCaptures pos
    quietMoves                        = getSortedQuietMoves pos
    hasSome [] = False
    hasSome _  = True


getSortedKillers :: (?killersTable :: KillersTable)
  => Ply -> Position -> IO [Move]
getSortedKillers !ply pos =
  sortMoves pos <$> killerMoves
  where
    killerMoves     =
      filter (`isLegalQuietMove` pos) <$> KillersTable.lookupMoves ply


getSortedCaptures :: Position -> ([Move], [Move])
getSortedCaptures pos =
  bimap (map fst) (map fst)
    $ partition ((>= 0) . snd)
    $ sortOn (Down . snd)
    $ map attachEval
    $ allCaptures pos
  where
    attachEval mv = (mv, evaluateCaptureExchange mv pos)


getSortedQuietMoves :: Position -> [Move]
getSortedQuietMoves pos =
  sortMoves pos $ allQuietMoves pos


sortMoves :: Position -> [Move] -> [Move]
sortMoves pos =
  sortOn (Down . getMoveScore)
  where
    getMoveScore mv = - evaluatePosition (makeMove mv pos)
