module Common where

import Data.Aeson
import Data.Maybe
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Rainbow
import Types
import LogShow
import DataSource

fmapAtIndex :: Int -> (a -> a) -> [a] -> [a]
fmapAtIndex n f ls = a ++ (f item:b)
  where (a, (item:b)) = splitAt n ls

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
            showLastTaskLog
            putChunkLn $ chunk "Previous task has not been stopped, not able to start new." & fore red
            error ""
    Left e -> error e


printStoppedTask :: IO ()
printStoppedTask = do
  putChunkLn $ chunk "Stopped task:" & fore green

printStartedTask :: IO ()
printStartedTask = do
  putChunkLn $ chunk "Started task:" & fore green

printAmendedTask :: IO ()
printAmendedTask = do
  putChunkLn $ chunk "Amended task:" & fore green

printResumeTask :: IO ()
printResumeTask = do
  putChunkLn $ chunk "Resuming task:" & fore green

getEndOfWeekDay :: Day -> Day
getEndOfWeekDay fromTimeDay = addDays
  (7 - (fromIntegral $ snd $ mondayStartWeek fromTimeDay))
  fromTimeDay
