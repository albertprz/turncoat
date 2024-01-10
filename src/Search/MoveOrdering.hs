module Search.MoveOrdering where

import           AppPrelude                hiding (union, (\\))

import           Evaluation.StaticExchange
import           Models.Move
import           Models.Position
import           Models.TranspositionTable (TTable)
import qualified Models.TranspositionTable as TTable
import           MoveGen.PieceCaptures     (allLegalCaptures)
import           MoveGen.PieceMoves        (allLegalMoves)


{-# INLINE  getSortedMoves #-}
getSortedMoves :: (?tTable :: TTable) => Position -> IO [Move]
getSortedMoves !pos = do
  ttMove <- TTable.lookupBestMove $ getZobristKey pos
  pure (toList ttMove <> winCaptures <> allMoves)
  where
    winCaptures = getSortedWinCaptures pos
    allMoves = allLegalMoves pos

{-# INLINE  getSortedWinCaptures #-}
getSortedWinCaptures :: Position -> [Move]
getSortedWinCaptures !pos =
  map fst
  $ sortOn (Down . snd)
  $ filter ((> 0) . snd)
  $ map addSee allCaptures
  where
    addSee !mv = (mv, evaluateCaptureExchange pos mv)
    allCaptures = getAllCaptures pos

{-# INLINE  getAllCaptures #-}
getAllCaptures :: Position -> [Move]
getAllCaptures !pos =
  allLegalCaptures pos.enemy pos
