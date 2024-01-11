module CommandLine.UciCommands where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Position
import           Models.Score
import qualified Models.TranspositionTable as TTable
import           MoveGen.MakeMove        
import           MoveGen.PieceMoves
import           Search.Perft
import           Search.Search
import           Search.SearchOptions

import           Control.Monad.State
import           Data.Composition          
import           Data.Map                 
import           System.TimeIt


printPerft :: Depth -> CommandM ()
printPerft = withPosition go
  where
    go = putStrLn
         . tshow
         .: perft

printDivide :: Depth -> CommandM ()
printDivide = withPosition go
  where
    go = void
         . traverseWithKey (\k v -> putStrLn (tshow k <> " => " <> tshow v))
         .: divide

printBestMove :: SearchOptions -> CommandM ()
printBestMove opts = do
  tTable <- liftIO TTable.create
  result <- withPosition (go tTable) opts.depth
  liftIO $ TTable.clear tTable
  pure result
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
            (allMoves pos)


data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }

data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }

type CommandM = StateT Position IO
