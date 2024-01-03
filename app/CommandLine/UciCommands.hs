module CommandLine.UciCommands where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Position
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove          (makeMove)
import           MoveGen.PieceMoves        (allLegalMoves)
import           Search.Perft
import           Search.Search
import           Search.SearchOptions


import           Control.Monad.State
import           Data.Composition          ((.:))
import           Data.Map                  (traverseWithKey)
import           System.TimeIt


printPerft :: Int -> CommandM ()
printPerft = withPosition go
  where
    go = putStrLn
         . tshow
         .: perft

printDivide :: Int -> CommandM ()
printDivide = withPosition go
  where
    go = void
         . traverseWithKey (\k v -> putStrLn (tshow k <> " => " <> tshow v))
         .: divide

printBestMove :: SearchOptions -> CommandM ()
printBestMove opts = do
  tTable <- liftIO $ TTable.create 100_000_000
  withPosition (go tTable) opts.depth
  where
    go tTable =
      ((putStrLn . foldMap (("bestmove " ++) . tshow)) <=< liftIO . timeIt)
      .: getBestMove
      where
        ?tTable = tTable


setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  case newPos of
    Just position -> put position
    Nothing       -> putStrLn "Error: Invalid Position"
  where
    newPos = foldM (flip makeUnknownMove) initialPosition moves

withPosition :: MonadState Position m => (a -> Position -> m b) -> a -> m b
withPosition f n = do
  f n =<< get

makeUnknownMove :: UnknownMove -> Position -> Maybe Position
makeUnknownMove UnknownMove {..} pos =
  (`makeMove` pos) <$> mv
  where
  mv = find (\x -> x.start == start && x.end == end)
            (allLegalMoves pos)


data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }

data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }

type CommandM = StateT Position IO
