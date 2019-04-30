module Common where

-- import Turtle (liftA2, UTCTime, Text)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Maybe
import Data.Time.Format
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Data.Text (Text)
import Rainbow
import Data.Bool
import Control.Applicative

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

encodeLogFile :: [LogEntry] -> IO ()
encodeLogFile v = do
  Data.ByteString.Lazy.Char8.writeFile logPath $ encodePretty v

recordIsStopped :: (a,b,c, Maybe d) -> Bool
recordIsStopped (_,_,_,endTime) = isJust endTime

addJsonArrayElementFile :: LogEntry -> FilePath -> IO ()
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

replaceLastJsonArrayElementFile :: LogEntry -> FilePath -> IO ()
replaceLastJsonArrayElementFile a fp = do
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

printAmendedTask :: LogEntry -> IO ()
printAmendedTask x = do
  putChunkLn $ chunk "Amended task:" & fore green
  print $ x

printResumeTask :: LogEntry -> IO ()
printResumeTask x = do
  putChunkLn $ chunk "Resuming task:" & fore green
  print $ x

printLastTask :: LogEntry -> IO ()
printLastTask x = do
  putStrLn "Last task:"
  print $ x

eitherDecodeLog :: IO (Either String [LogEntry])
eitherDecodeLog = eitherDecodeFileStrict logPath

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

filterValidEntries :: [LogEntry] -> [Entry]
filterValidEntries [] = []
filterValidEntries (x:xs) =
  validLogEntry x ++ filterValidEntries xs
  where
    validLogEntry (a,b,c, Just d) = [Entry a b c d]
    validLogEntry (_,_,_, Nothing) = []

validEntries :: IO (Either String [Entry])
validEntries = (fmap filterValidEntries) <$> eitherDecodeLog

allEntries :: IO (Either String [Entry])
allEntries = (liftA2 (++)) <$> getLatestEntryInProgress <*> validEntries

getLatestEntryInProgress :: IO (Either String [Entry])
getLatestEntryInProgress = do
  ct <- getCurrentTime
  x <- eitherDecodeLog
  case x of 
    Right x' ->
      case last x' of
        (a,b,c, Nothing) -> pure . pure $ [Entry a b c ct]
        _ -> pure . pure $ []
    Left e -> pure $ Left e

bar :: Double -> String
bar x = --flip take "▁▂▃▄▅▆▇█"
     b8 (v8)
  ++ bool "" " " (length (b8 v8) > 0)
  ++ b6 (v6)
  ++ bool "" " " (length (b6 v6) > 0)
  ++ b4 (v4)
  ++ bool "" " " (length (b4 v4) > 0)
  ++ b2 (v2)
  where
  v8 = f 0 (8/8)
  v6 = f (fromIntegral v8) (6/8)
  v4 = f (fromIntegral v8 + (fromIntegral v6 * (6/8))) (4/8)
  v2 = f (fromIntegral v8 + (fromIntegral v6 * (6/8)) + (fromIntegral v4 * (4/8))) (2/8)
  f x' y' = floor ((x - x') / y')
  b8 = duplicate "█ " -- "8"
  b6 = duplicate "▆ " -- "6"
  b4 = duplicate "▄ " -- "4"
  b2 = duplicate "▂ " -- "2"

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

getEndOfWeekDay :: Day -> Day
getEndOfWeekDay fromTimeDay = addDays
  (7 - (fromIntegral $ snd $ mondayStartWeek fromTimeDay))
  fromTimeDay
