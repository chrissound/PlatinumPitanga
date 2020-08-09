{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module LogShow where

import           Types
import           DataSource
import           Time

import           Control.Lens.Operators
import           Data.Vector                    ( slice
                                                , fromList
                                                , toList
                                                )
import           Control.Monad
import           Data.Time.Clock
import           Data.Time.Calendar
import           Data.Time.Calendar.OrdinalDate
import           Data.String.Conversions
import           Rainbow
import           Data.List
import           Data.Function
import           Data.Text                      ( Text )
import           Data.Strings
import           Data.Bool
import           Data.Time.Format
import           Data.String
-- import           Text.Pretty.Simple

showLastTaskLog :: IO ()
showLastTaskLog = showTaskLog (PitangaLogCommand (Just $ First 1) Nothing)


showTaskLogGrouped :: GroupBy -> [LogEntry] -> IO ()
showTaskLogGrouped (GroupBy _) zzz = do
  ct <- getCurrentTime
  let t     = fmap leToLe' zzz
  -- print $ z 
  fff <- byteStringMakerFromEnvironment
  let t' = reverse $ groupBy ((==) `on` ((,) <$> utctDay . start' <*> task' )) (sortBy (compare `on` start') t)
  -- let final = sort $ zip ((sum . diffsTotal) <$> t') (task . head <$> t')
  -- pPrint t
  let
    fprint x = (chunksToByteStrings fff $
          [ stringCol blue  $ cs $ myFormatDiffTime $ sum $ diffsTotal $ (logEntryToEntry ct <$> x)
          , chunk " | "
          , stringCol yellow $ show $ utctDay (start' $ head x)
          , chunk "\n"
          ])
          <> 
          mconcat (chunksToByteStrings fff . xyz <$> x) 
          <>
          chunksToByteStrings fff [chunk "\n"]
            -- [
              -- mconcat $ xyz <$> x
            -- , chunk "\n"
            -- ]
    xyz x = 
        [ 
          stringCol blue  $ cs $ myFormatDiffTime $ diffUTCTime (maybe ct id $ end' x) (start' x) 
        , chunk " | "
        , chunk $ cs $ task' x
        , chunk $ "\n"
        ]
  -- putChunkLn $ stringCol white "test"
  -- putChunkLn $ stringCol blue "test"
  -- putChunkLn $ stringCol red "test"

  -- mapM_ putChunkLn 
    -- [ stringCol white "test"
    -- , stringCol blue "test"
    -- , stringCol red "test"
    -- ]
  putStrLn $ cs $ mconcat $ mconcat $ (fprint) <$> t'
  putStrLn ""
  putStrLn "--------------------------------------------"


showTaskLog :: PitangaLogCommand -> IO ()
showTaskLog (PitangaLogCommand limit gp) = do
  eitherDecodeLog >>= \case
    Right x -> do
      let zzz =
            (case limit of
                Just (First f) -> toList . slice 0 f . fromList
                Just (Last  f) -> toList . slice (length x - f) (f) . fromList
                Nothing        -> id
              )
              $ reverse x
      case gp of
        Nothing -> showTaskLog' zzz
        Just x' -> showTaskLogGrouped x' zzz
    Left e -> error e


showTaskLog'
  :: (Foldable t, ConvertibleStrings a String, Monoid a, Data.String.IsString a)
  => t (a, a, UTCTime, Maybe UTCTime)
  -> IO ()
showTaskLog' zzz = do
  forM_ (zzz) $ \(t, d, s, e) -> do
    case e of
      Just e' -> do
        case (utctDay s == utctDay e') of
          True -> do
            putTimeEntry
              Nothing
              (  mconcat [myFormatUtcTimeOnly s, " -> ", myFormatUtcTimeOnly e']
              ++ "\n"
              ++ myFormatUtcDateOnly s
              )
              (myFormatDiffTimeBars s e')
          False -> do
            putTimeEntry
              (Just "!!! Overnight?")
              (mconcat [myFormatUtcTime s, " -> ", myFormatUtcTime e'])
              (myFormatDiffTimeBars s e')
      Nothing -> do
        ct <- getCurrentTime
        putInProgress "IN PROGRESS"
        putTimeEntry Nothing
                     (mconcat [myFormatUtcTime s, " -> ---"])
                     (myFormatDiffTimeBars s ct)
    putTaskDesc $ cs $ mconcat [t, " - ", d, "\n"]

showLog :: PitangaLogCommand -> IO ()
showLog x = do
  showWeekLogSummary
  showLogSummary
  showLogWeekAggregate
  showTaskLog x

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

myFormatDiffTimeBars :: UTCTime -> UTCTime -> [Char]
myFormatDiffTimeBars s e =
  (myFormatDiffTime $ diffUTCTime e s) ++ " " ++ getBarsInHours
    (fromIntegral $ floor $ diffUTCTime e s)

showWeekLogSummary :: IO ()
showWeekLogSummary = do
  ct <- getCurrentTime
  allEntries >>= \case
    Right x -> do
      forM_ (reverse [1 .. 6]) $ \i -> do
        let ddd = addDays (-i) (utctDay ct)
        let totalDurationDdd =
              sum $ diffsTotal (getDurationForEntriesOnDay ddd $ x)
        putTotalTime
          $  (formatTime defaultTimeLocale "%a" ddd)
          ++ ":   "
          ++ myFormatDiffTime totalDurationDdd
          ++ " "
          ++ (getBarsInHours $ fromIntegral $ floor totalDurationDdd)
    Left e -> error e

showLogSummary :: IO ()
showLogSummary = allEntries >>= \case
  Right x -> do
    ct <- getCurrentTime
    let totalDurationToday =
          sum $ diffsTotal (getDurationForEntriesOnDay (utctDay ct) $ x)
    let totalDurationWeek = sum $ fmap snd $ getDurationPerDayInWeek x ct
    putStrLn "--------------------------------------------"
    putTotalTime
      $  "Today: "
      ++ myFormatDiffTime totalDurationToday
      ++ " "
      ++ (getBarsInHours $ fromIntegral $ floor totalDurationToday)
    putStrLn "--------------------------------------------"
    putTotalTime
      $  "Week:  "
      ++ myFormatDiffTime totalDurationWeek
      ++ " "
      ++ (getBarsForWeek $ fromIntegral $ floor totalDurationWeek)
    putStrLn "--------------------------------------------"
    putStrLn ""
  Left e -> error e

showLogWeekAggregate :: IO ()
showLogWeekAggregate = allEntries >>= \case
  Right x -> do
    ct <- getCurrentTime
    let t = filter
          ( betweenUTCTime (UTCTime (relativeLastMonday $ utctDay ct) 0)
                           (UTCTime (utctDay ct) 86400)
          . start
          )
          x
    let t'    = groupBy ((==) `on` task) (sortBy (compare `on` task) t)
    let final = sort $ zip ((sum . diffsTotal) <$> t') (task . head <$> t')
    let
      fprint (a, b) =
        ( stringCol white
          . cs
          . strPadRight ' ' 44
          $ (take 43 :: String -> String) (cs b)
          )
          <> chunk " "
          <> stringCol blue (cs $ myFormatDiffTime a)
          <> chunk " "
          <> (chunk $ getBarsInHours $ fromIntegral $ floor a)
    mapM_ (putChunkLn . fprint) final
    putStrLn ""
    putStrLn "--------------------------------------------"
  Left e -> error e

stringCol :: Radiant -> String -> Chunk String
stringCol c s = ((chunk s & fore c) :: Chunk String)


getBarsInHours :: Double -> String
getBarsInHours x = bar $ (x * 2) / (60 * 60)

getBarsForWeek :: Double -> String
getBarsForWeek x = bar $ x / (60 * 60 * 8)

diffsTotal :: Functor f => f Entry -> f NominalDiffTime
diffsTotal = fmap (diffUTCTime <$> end <*> start)

getDurationForEntriesOnDay :: Day -> [Entry] -> [Entry]
getDurationForEntriesOnDay d =
  filter (betweenUTCTime (UTCTime d 0) (UTCTime d 86400) . start)

getDurationPerDayInWeek :: [Entry] -> UTCTime -> WeekEntry
getDurationPerDayInWeek v ct = do
  let daysInCurrentWeek = snd $ mondayStartWeek (utctDay ct)
  reverse
    $   [0 .. (daysInCurrentWeek - 1)]
    <&> (\x -> do
          let day = addDays (fromIntegral $ negate x) (utctDay ct)
          (,) day
            $ sum
            . fmap (diffUTCTime <$> end <*> start)
            $ getDurationForEntriesOnDay day v
        )

betweenUTCTime :: UTCTime -> UTCTime -> UTCTime -> Bool
betweenUTCTime = between

between :: (Ord a) => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)


relativeLastMonday :: Day -> Day
relativeLastMonday x = fromMondayStartWeek
  ((\(v', _, _) -> v') $ toGregorian x)
  x'
  1
  where (x', _) = (mondayStartWeek x)

bar :: Double -> String
bar x = b8 (v8) ++ b6 (v6) ++ b4 (v4) ++ b2 (v2)
 where
  v8 = f 0 (8 / 8)
  v6 = f (fromIntegral v8) (6 / 8)
  v4 = f (fromIntegral v8 + (fromIntegral v6 * (6 / 8))) (4 / 8)
  v2 = f
    (fromIntegral v8 + (fromIntegral v6 * (6 / 8)) + (fromIntegral v4 * (4 / 8))
    )
    (2 / 8)
  f x' y' = floor ((x - x') / y')
  b8 = duplicate "█" -- "8"
  b6 = duplicate "▌" -- "6"
  b4 = duplicate "▎" -- "4"
  b2 = duplicate "" -- "2"

bar' :: Double -> String
bar' x = --flip take "▁▂▃▄▅▆▇█"
  b8 (v8)
    ++ bool "" " " (length (b8 v8) > 0)
    ++ b6 (v6)
    ++ bool "" " " (length (b6 v6) > 0)
    ++ b4 (v4)
    ++ bool "" " " (length (b4 v4) > 0)
    ++ b2 (v2)
 where
  v8 = f 0 (8 / 8)
  v6 = f (fromIntegral v8) (6 / 8)
  v4 = f (fromIntegral v8 + (fromIntegral v6 * (6 / 8))) (4 / 8)
  v2 = f
    (fromIntegral v8 + (fromIntegral v6 * (6 / 8)) + (fromIntegral v4 * (4 / 8))
    )
    (2 / 8)
  f x' y' = floor ((x - x') / y')
  b8 = duplicate "█ " -- "8"
  b6 = duplicate "▆ " -- "6"
  b4 = duplicate "▄ " -- "4"
  b2 = duplicate "▂ " -- "2"

duplicate :: String -> Int -> String
duplicate string n = concat $ replicate n string

filterByDay :: Day -> [Entry] -> [Entry]
filterByDay d = filter (betweenUTCTime (UTCTime d 0) (UTCTime d 86400) . start)

exportGroupByTask' :: [Entry] -> Day -> [(NominalDiffTime, Text)]
exportGroupByTask' entries d = do
  let t  = filterByDay d entries
  let t' = groupBy ((==) `on` task) (sortBy (compare `on` task) t)
  sort $ zip ((sum . diffsTotal) <$> t') (task . head <$> t')

exportGroupByTask :: Day -> IO [(Day, [(NominalDiffTime, Text)])]
exportGroupByTask d = allEntries >>= \case
  Right x -> do
    ct <- getCurrentTime
    let z = (\x'' -> addDays (x'') d) <$> [0 .. diffDays (utctDay ct) d]
    pure $ zip z (exportGroupByTask' x <$> z)
  Left e -> error e
