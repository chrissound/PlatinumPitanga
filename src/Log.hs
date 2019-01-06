{-# LANGUAGE OverloadedStrings #-}
module Log where

import Control.Monad
import Data.String.Conversions
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Control.Lens.Operators
import Rainbow
import Control.Applicative

import Common

-- import MyDebug

betweenUTCTime :: UTCTime -> UTCTime -> UTCTime -> Bool
betweenUTCTime a b x = (a <= x) && (x <= b)

getDurationForEntriesOnDay :: Day -> [Entry] -> [Entry]
getDurationForEntriesOnDay d = filter (betweenUTCTime (UTCTime d 0) (UTCTime d 86400) . start)

getDurationPerDayInWeek :: [Entry] -> UTCTime -> WeekEntry
getDurationPerDayInWeek v ct = do
      let daysInCurrentWeek = snd $ mondayStartWeek (utctDay ct)
      reverse $ [0..(daysInCurrentWeek - 1)] <&> (\x ->
               do
                 let day = addDays (fromIntegral $ negate x) (utctDay ct)
                 (,) day
                   $ sum . fmap (diffUTCTime <$> end <*> start) $ getDurationForEntriesOnDay day v
            )

getLatestEntryInProgress :: IO (Either String Entry)
getLatestEntryInProgress = do
  ct <- getCurrentTime
  fmap last
    . (fmap filterValidEntries)
    . fmap (fmap (\(a,b,c,d) -> (a,b,c, case d of Just _ -> d; Nothing -> Just ct)))
    <$> eitherDecodeLog

showLog :: IO ()
showLog = do
  (liftA2 (:)) <$> getLatestEntryInProgress <*> validEntries >>= \case
    Right x -> do
      ct <- getCurrentTime
      let totalDurationToday = sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay ( utctDay ct) $ x)
      let totalDurationWeek = sum $ fmap snd $ getDurationPerDayInWeek (x) ct
      putStrLn "--------------------------------------------"
      putTotalTime $ "Today: "
        ++ myFormatDiffTime totalDurationToday
        ++ " "
        ++ (getBarsInHours $ fromIntegral $ floor totalDurationToday)
      putStrLn "--------------------------------------------"
      putTotalTime $ "Week:  "
        ++ myFormatDiffTime totalDurationWeek
        ++ " "
        ++ (getBarsForWeek $ fromIntegral $ floor totalDurationWeek)
      putStrLn "--------------------------------------------"
      putStrLn ""
    Left e -> error e
  eitherDecodeLog >>= \case
    Right x -> do
      forM_ (reverse x) $ \(t,d,s,e) -> do
        case e of
          Just e' -> do
            case (utctDay s == utctDay e') of
              True -> do
                putTimeEntry Nothing
                  (mconcat [myFormatUtcTimeOnly s, " - ", myFormatUtcTimeOnly e'] ++ "\n" ++ myFormatUtcDateOnly s)
                  (myFormatDiffTimeBars s e')
              False -> do
                putTimeEntry (Just "!!! Overnight?") 
                  (mconcat [myFormatUtcTime s, " - ", myFormatUtcTime e'])
                  (myFormatDiffTimeBars s e')
                  --((myFormatDiffTime $ diffUTCTime e' s) ++ " " ++ getBarsInHours (diffUTCTime e' s))
          Nothing -> do
            ct <- getCurrentTime
            putInProgress "IN PROGRESS"
            putTimeEntry Nothing
              (mconcat [myFormatUtcTime s, " - ---"])
              (myFormatDiffTimeBars s ct)
        putTaskDesc $ cs $ mconcat [t, " - ", d, "\n"]
    Left e -> error e

myFormatDiffTimeBars :: UTCTime -> UTCTime -> [Char]
myFormatDiffTimeBars s e = (myFormatDiffTime $ diffUTCTime e s) ++ " " ++ getBarsInHours (fromIntegral $ floor $ diffUTCTime e s)

getBarsInHours :: Double -> String
getBarsInHours x = bar $ (x * 2) / (60 * 60)

getBarsForWeek :: Double -> String
getBarsForWeek x = bar $ x / (60 * 60 * 8)

stringCol :: Radiant -> String -> Chunk String
stringCol c s = ((chunk s & fore c) :: Chunk String)

putTimeEntry :: Maybe String -> String -> String -> IO ()
putTimeEntry e startEnd duration = do
  case e of
    Just e' -> putChunkLn $ stringCol red $ e'
    Nothing -> pure ()
  putChunkLn $ stringCol yellow $ startEnd
  putChunkLn $ stringCol blue $ duration

putTaskDesc :: String -> IO ()
putTaskDesc desc = do
  putStrLn desc

putInProgress :: String -> IO ()
putInProgress t = do
  putChunkLn $ stringCol green $ t

putTotalTime :: String -> IO ()
putTotalTime t = do
  putChunkLn $ stringCol yellow $ t
