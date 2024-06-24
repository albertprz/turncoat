module Search.TimeManagement (MicroSeconds, getMoveTime, maybeTimeout, isTimeOver, (|-|))  where

import           AppPrelude                 hiding (timeout)
import           Control.Concurrent.Timeout (timeout)
import           Models.Command
import           Models.Piece

import           Data.Time.Clock.System


getMoveTime :: SearchOptions -> Color -> Maybe MicroSeconds
getMoveTime SearchOptions {..} color =
  fromIntegral . (* 1000) <$> (moveTime <|> timeToMove)
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
maybeTimeout (Just duration) action =
  timeout (fromIntegral duration) action $> ()
maybeTimeout Nothing action         =
  action


isTimeOver :: SystemTime -> SystemTime -> Maybe MicroSeconds -> Bool
isTimeOver endTime startTime (Just moveTime) =
  endTime |-| startTime > getTimeOver moveTime
isTimeOver _ _ Nothing =
  False


getTimeOver :: MicroSeconds -> MicroSeconds
getTimeOver moveTime =
  moveTime * 2 / 5


infixl 9 |-|
(|-|) :: SystemTime -> SystemTime -> MicroSeconds
(|-|) endTime startTime =
  systemTimeToMicros endTime - systemTimeToMicros startTime
  where
  systemTimeToMicros MkSystemTime {..} =
    fromIntegral systemSeconds * 1_000_000
    + fromIntegral systemNanoseconds / 1000


type MicroSeconds = Word64
