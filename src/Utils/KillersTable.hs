module Utils.KillersTable where

import           AppPrelude

import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MoveQueries

import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector


type KillersTable = IOVector StorableMove


lookupMoves :: (?killersTable :: KillersTable) => Ply -> IO [Move]
lookupMoves !ply = do
  firstMove  <- Vector.unsafeRead ?killersTable idx
  secondMove <- Vector.unsafeRead ?killersTable (idx + 1)
  pure $ mapMaybe decodeMove [firstMove, secondMove]
  where
    !idx = killerSlots * fromIntegral ply


insert :: (?killersTable :: KillersTable) => Ply -> Position -> Move -> IO ()
insert !ply pos mv =
  when (isQuietMove mv pos) do
  firstMove  <- Vector.unsafeRead ?killersTable idx
  unless (mv `elem` decodeMove firstMove) do
    Vector.unsafeWrite ?killersTable idx
                                    (encodeMove $ Just mv)
    Vector.unsafeWrite ?killersTable (idx + 1)
                                     firstMove
  where
    !idx = killerSlots * fromIntegral ply


create :: IO KillersTable
create = Vector.replicate (killerSlots * (fromIntegral (maxBound @Ply) + 1))
                          (encodeMove Nothing)


clear ::  KillersTable -> IO ()
clear = Vector.clear


killerSlots :: Int
killerSlots = 2
