{-# LANGUAGE OverloadedStrings #-}
module Log where

import Control.Monad
import Data.String.Conversions
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Control.Lens.Operators

import Common

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

showLog :: IO ()
showLog = do
  ct <- getCurrentTime
  eitherDecodeLog >>= \case
    Right x -> do
      putStrLn $ "Today "
        ++ (myFormatDiffTime $ sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay ( utctDay ct) $ filterValidEntries x))
      putStrLn $ "Week  "
        ++ myFormatDiffTime (sum $ fmap snd $ getDurationPerDayInWeek (filterValidEntries x) ct)
      putStrLn ""
      forM_ (reverse x) $ \(t,d,s,e) -> do
        case e of
          Just e' -> do
            putStrLn $ mconcat [myFormatUtcTime s, " - ", myFormatUtcTime e']
            putStrLn $ myFormatDiffTime $ diffUTCTime e' s
          Nothing -> putStrLn $ mconcat [myFormatUtcTime s, " - ---"]
        putStrLn $ cs $ mconcat [t, " - ", d, "\n"]
    Left e -> error e
