module Utils.DateTime  ( formatDbDateTime, minutesFromTimeOfDay, stringToTimeOfDay) where

import Data.Time.Format
import Data.Time (FormatTime)
import Data.Time


formatDbDateTime :: FormatTime t => t -> String
formatDbDateTime =
  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

minutesFromTimeOfDay :: TimeOfDay -> Int
minutesFromTimeOfDay t =
  todHour t * 60 + todMin t

stringToTimeOfDay :: String -> TimeOfDay
stringToTimeOfDay value =
  let hour = read (take 2 value) :: Int
      minute = read (drop 3 value) :: Int
   in TimeOfDay hour minute 0