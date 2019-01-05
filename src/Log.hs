{-# LANGUAGE OverloadedStrings #-}
module Log where

import Control.Monad
import Data.String.Conversions
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Control.Lens.Operators
import Rainbow

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
  validEntries >>= \case
    Right x -> do
      ct <- getCurrentTime
      putStrLn $ "Today "
        ++ (myFormatDiffTime $ sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay ( utctDay ct) $ x))
      putStrLn $ "Week  "
        ++ myFormatDiffTime (sum $ fmap snd $ getDurationPerDayInWeek (x) ct)
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
                  (myFormatDiffTime $ diffUTCTime e' s)

              False -> do
                putTimeEntry (Just "!!! Overnight?") 
                  (mconcat [myFormatUtcTime s, " - ", myFormatUtcTime e'])
                  (myFormatDiffTime $ diffUTCTime e' s)
          Nothing -> do
            ct <- getCurrentTime
            putInProgress "IN PROGRESS"
            putTimeEntry Nothing
              (mconcat [myFormatUtcTime s, " - ---"])
              (myFormatDiffTime $ diffUTCTime ct s)
        putTaskDesc $ cs $ mconcat [t, " - ", d, "\n"]
    Left e -> error e

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


getBarVal :: Int -> String
getBarVal = flip take "▁▂▃▄▅▆▇█"
