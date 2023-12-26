module CommandLine.UciCommands where

import           AppPrelude

import           Constants.Boards
import           Models.Move
import           Models.Position
import           MoveGen.MakeMove    (playMove)
import           MoveGen.PieceMoves  (allLegalMoves)
import           Search.Perft
import           Search.Search
import           Search.SearchOptions


import           Control.Monad.State
import           Data.Composition    ((.:))
import           Data.Map            (traverseWithKey)


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
printBestMove opts = withPosition go opts.depth
  where
    go =  putStrLn
         . foldMap (("Best move: " ++ ) . tshow)
         .: getBestMove 
    

setPosition :: PositionSpec -> CommandM ()
setPosition PositionSpec {..} =
  case newPos of
    Just position -> put position
    Nothing       -> putStrLn "Error: Invalid Position"
  where
    newPos = foldM (flip playUnknownMove) initialPosition moves

withPosition :: MonadState Position m => (a -> Position -> m b) -> a -> m b
withPosition f n = do
  position <- get
  f n position

playUnknownMove :: UnknownMove -> Position -> Maybe Position
playUnknownMove UnknownMove {..} pos =
  (`playMove` pos) <$> mv
  where
  mv = find (\x -> x.start == start && x.end == end)
            (allLegalMoves pos)

type CommandM = StateT Position IO

data PositionSpec = PositionSpec
  { initialPosition :: Position,
    moves           :: [UnknownMove]
  }

data UnknownMove = UnknownMove
  { start :: Square,
    end   :: Square
  }

  
