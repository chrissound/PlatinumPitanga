module Time where

import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX

myFormatUtcTime :: UTCTime -> String
myFormatUtcTime = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y"

myFormatUtcTimeSpeardsheetFriendly :: FormatTime t => t -> String
myFormatUtcTimeSpeardsheetFriendly = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y"

myFormatUtcTimeOnly :: UTCTime -> String
myFormatUtcTimeOnly = formatTime defaultTimeLocale "%H:%M:%S"

myFormatUtcDateOnly :: FormatTime t => t -> String
myFormatUtcDateOnly = formatTime defaultTimeLocale "%d/%m/%Y"

myFormatDiffTime :: NominalDiffTime -> String
myFormatDiffTime v =
  case formatTime defaultTimeLocale "%d" $ posixSecondsToUTCTime v of
    "01" -> vvv
    x -> mconcat ["days (", x, ")!!! ", vvv]
    where vvv = formatTime defaultTimeLocale "%H:%M:%S" $ posixSecondsToUTCTime v
