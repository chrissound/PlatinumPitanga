module Common where

import Turtle (UTCTime, Text)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Maybe
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Rainbow
import Data.Time.Calendar

type LogEntry = (Text, Text, UTCTime, Maybe (UTCTime))
type WeekEntry = [(Day, NominalDiffTime)]
data Entry = Entry {
    task :: Text
  , description :: Text
  , start :: UTCTime
  , end :: UTCTime
} deriving Show

decodeLog :: IO (Either String [LogEntry])
decodeLog = eitherDecodeFileStrict logPath

fmapAtIndex :: Int -> (a -> a) -> [a] -> [a]
fmapAtIndex n f ls = a ++ (f item:b)
  where (a, (item:b)) = splitAt n ls

logPath :: FilePath
logPath = "log.json"

encodeLogFile :: ToJSON a => a -> IO ()
encodeLogFile v = do
  Data.ByteString.Lazy.Char8.writeFile logPath $ encodePretty v

recordIsStopped :: (a,b,c, Maybe d) -> Bool
recordIsStopped (_,_,_,endTime) = isJust endTime

addJsonArrayElementFile :: LogEntry -> String -> IO ()
addJsonArrayElementFile a fp = do
  eitherDecodeFileStrict fp >>= \case
    Right x -> do
      if (length x == 0) || (recordIsStopped $ last x)
        then
          encodeLogFile $ x ++ [a]
        else
          do
            printLastTask $ last x
            putChunkLn $ chunk "Previous task has not been stopped, not able to start new." & fore red
            error ""
    Left e -> error e

printStoppedTask :: LogEntry -> IO ()
printStoppedTask x = do
  putChunkLn $ chunk "Stopped task:" & fore green
  print $ x

printStartedTask :: LogEntry -> IO ()
printStartedTask x = do
  putChunkLn $ chunk "Started task:" & fore green
  print $ x

printLastTask :: LogEntry -> IO ()
printLastTask x = do
  putStrLn "Last task:"
  print $ x

eitherDecodeLog :: IO (Either String [LogEntry])
eitherDecodeLog = eitherDecodeFileStrict logPath

myFormatUtcTime :: UTCTime -> String
myFormatUtcTime = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y"

myFormatUtcTimeSpeardsheetFriendly :: UTCTime -> String
myFormatUtcTimeSpeardsheetFriendly = formatTime defaultTimeLocale "%H:%M:%S %d/%m/%Y"

myFormatUtcTimeOnly :: UTCTime -> String
myFormatUtcTimeOnly = formatTime defaultTimeLocale "%H:%M:%S"

myFormatUtcDateOnly :: UTCTime -> String
myFormatUtcDateOnly = formatTime defaultTimeLocale "%d/%m/%Y"

myFormatDiffTime :: NominalDiffTime -> String
myFormatDiffTime = formatTime defaultTimeLocale "%H:%M:%S" . posixSecondsToUTCTime

filterValidEntries :: [LogEntry] -> [Entry]
filterValidEntries [] = []
filterValidEntries (x:xs) =
  validLogEntry x ++ filterValidEntries xs
  where
    validLogEntry (a,b,c, Just d) = [Entry a b c d]
    validLogEntry (_,_,_, Nothing) = []

validEntries :: IO (Either String [Entry])
validEntries = (fmap filterValidEntries) <$> eitherDecodeLog
