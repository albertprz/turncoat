module Search.TimeManagement (getMoveTime, maybeTimeout)  where

import           AppPrelude                 hiding (timeout)
import           Control.Concurrent.Timeout (timeout)
import           Models.Command
import           Models.Piece


getMoveTime :: SearchOptions -> Color -> Maybe MicroSeconds
getMoveTime SearchOptions {..} color =
  map (fromIntegral . (* 1000))
      (moveTime <|> timeToMove)
  where
    timeToMove
      | Just t <- time
      , Just i <- inc
        = Just (t / movesUntil + i)
      | Just t <- time
        = Just (t / movesUntil)
      | Just i <- inc
        = Just i
      | otherwise
        = Nothing
    (time, inc)
      | White <- color = (whiteTime, whiteIncrement)
      | Black <- color = (blackTime, blackIncrement)
    movesUntil = fromMaybe 40 movesUntilNextTime


maybeTimeout :: Maybe MicroSeconds -> IO () -> IO ()
maybeTimeout (Just duration) action = timeout duration action $> ()
maybeTimeout Nothing action         = action


type MicroSeconds = Integer
