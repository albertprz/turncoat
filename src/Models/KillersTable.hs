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
  !firstMove <- decodeMove <$> Vector.unsafeRead ?killersTable idx
  !secondMove <- decodeMove <$> Vector.unsafeRead ?killersTable (idx + 1)
  pure (catMaybes [firstMove, secondMove])
  where
    !idx = 2 * fromIntegral ply


{-# INLINE  insert #-}
insert :: (?killersTable :: KillersTable) => Ply -> Position -> Move -> IO ()
insert !ply pos move = when (isQuietMove move pos) do
  !firstMove <- decodeMove <$> Vector.unsafeRead ?killersTable idx
  let insertIdx = if all (== move) firstMove
                    then idx
                    else idx + 1
  Vector.unsafeWrite ?killersTable insertIdx
                                   (encodeMove $ Just move)
  where
    !idx = 2 * fromIntegral ply


create :: IO KillersTable
create = Vector.replicate (fromIntegral (4 * (maxBound @Word8)))
                          (encodeMove Nothing)


clear ::  KillersTable -> IO ()
clear = Vector.clear
