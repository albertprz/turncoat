module Search.MoveOrdering where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.Score
import           Models.TranspositionTable (TEntry (..), TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove
import           MoveGen.PieceMoves


getSortedMoves :: (?tTable :: TTable) => Int -> Position -> IO [Move]
getSortedMoves depth pos
  | depth <= 2 = pure $ allLegalMoves pos
  | otherwise = do
    (!hashed, !nonHashed) <- partitionTraverse lookupMove $ allLegalMoves pos
    let (!pvHashed, !nonPvHashed) = partition ((== PV) . (.nodeType) . snd) hashed
    pure $! (map fst pvHashed ++ map fst nonPvHashed ++ nonHashed)
    where
      lookupMove move = map (move,)
        <$$> TTable.lookup depth $ getZobristKey $ makeMove move pos
