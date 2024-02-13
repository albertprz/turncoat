module Models.KillersTable where

import           AppPrelude

import           Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as Vector
import           Models.Move
import           Models.Position
import           Models.Score
import           MoveGen.MoveQueries          (isQuietMove)


type KillersTable = IOVector StorableMove


{-# INLINE  lookupMoves #-}
lookupMoves :: (?killersTable :: KillersTable) => Ply -> IO [Move]
lookupMoves !ply = do
  !firstMove  <- decodeMove <$> Vector.unsafeRead ?killersTable idx
  !secondMove <- decodeMove <$> Vector.unsafeRead ?killersTable (idx + 1)
  pure $ catMaybes [firstMove, secondMove]
  where
    !idx = killerSlots * fromIntegral ply


{-# INLINE  insert #-}
insert :: (?killersTable :: KillersTable) => Ply -> Position -> Move -> IO ()
insert !ply pos mv = when (isQuietMove mv pos) do
  !firstMove <- Vector.unsafeRead ?killersTable idx
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
killerSlots = 4
