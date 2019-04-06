{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wno-unused-imports #-}
module Export where

import Control.Monad
import Data.String.Conversions
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.OrdinalDate
import Control.Lens.Operators
import Data.Aeson
import Options.Applicative
import Control.Monad (join)
import Data.Monoid ((<>))
import Text.Pretty.Simple (pPrint)
import Data.Time
import Data.Time.Calendar
import Data.Text (Text)

import Common
import Log

data Export = ExportRawJson | ExportGroupByDay | ExportGroupByDayAndTask
data Export2 = Export2 (Export) (Maybe String)

exportEntries :: [Entry] -> IO ()
exportEntries x = do
  encodeFile "log_export.json"
    $  ("Task","desc","start","end", "duration")
        : (fmap
          (\(Entry t d s e) ->
              ( t
              , d
              , myFormatUtcTimeSpeardsheetFriendly s
              , myFormatUtcTimeSpeardsheetFriendly e
              , myFormatDiffTime $ diffUTCTime e s
            )
          )
          x
      )

exportEntries' :: [(Day, NominalDiffTime, Text)] -> IO ()
exportEntries' x = do
  encodeFile "log_export_group_by_taskday.json"
    $  ("Task","Date","Duration")
        : (fmap
          (\(a,b,c) ->
              ( c
              , myFormatUtcDateOnly a
              , myFormatDiffTime $ b
            )
          )
          x
      )

export :: IO ()
export = do
  validEntries >>= \case
    Right x -> exportEntries x
    Left e -> error e

exportWeek  :: IO ()
exportWeek = do
  validEntries >>= \case
    Right x -> exportEntries $ filter (const True) x
    Left e -> error e

-- work'  :: Bool -> Bool -> Bool -> Maybe String -> IO ()
-- work' = work''

work'' :: Bool -> Bool -> Bool -> Maybe String -> Export2
work'' True _ _ x = Export2 ExportRawJson x
work'' _ True _ x = Export2 ExportGroupByDay x
work'' _ _ True x = Export2 ExportGroupByDayAndTask x
work'' _ _ _ _   = error "???"

-- main :: IO ()
-- main = join . customExecParser (prefs showHelpOnError) $
--   info (helper <*> parser)
--   (  fullDesc
--   <> header "list directories"
--   )
--   where
--     parser :: Parser (IO ())
--     parser =
--       parser''' work


parser :: Parser Export2
parser =
        (work'') <$>
            switch
            (  long "raw-json"
            <> help "Export raw values as JSON"
            <> showDefault
            )
        <*>
            switch
            (  long "group-by-day"
            <> help "Sum aggregate group by day"
            <> showDefault
            )
        <*>
            switch
            (  long "group-by-day-and-task"
            <> help "Sum aggregate group by day and task"
            <> showDefault
            )
        <*>
          (
            optional
            $ strOption
            (  long "from"
            <> help "dd mm yy"
            <> showDefault
            )
          )

work :: Export2 -> IO ()
work (Export2 ExportRawJson _) = do
  exportWeek
  print "Exported this week successful"
work (Export2 ExportGroupByDayAndTask ft) = do
  case ft of
    Just ft' -> do
      let fromTime = parseTimeOrError True defaultTimeLocale "%-d %-m %Y" ft' :: UTCTime
      print $ "From: " <> show fromTime
      z <- exportGroupByTask $ utctDay fromTime
      exportEntries' $ mconcat $ (\(a,b) -> zip3 (repeat a ) (fst <$> b) (snd <$> b)) <$> z
      print "Export success"
    Nothing -> error "???"
work (Export2 ExportGroupByDay ft) = do
  ct <- getCurrentTime
  let cd = utctDay ct
  case ft of
    Just ft' -> do
      let fromTime = parseTimeOrError True defaultTimeLocale "%-d %-m %Y" ft' :: UTCTime
      let fromTimeDay = utctDay fromTime
      validEntries >>= \case
        Right x -> do
          print $ "From: " <> show fromTime
          let endOfWeek = getEndOfWeekDay fromTimeDay
          let currentEndOfWeek = getEndOfWeekDay cd
          let wCount = ceiling (fromIntegral (diffDays currentEndOfWeek endOfWeek) / 7.0)
          let s = reverse $ (\ss' -> addDays (negate $ ss' * 7) currentEndOfWeek) <$> [0..wCount]

          forM_ s (\s' -> do
            putStrLn "-----------"
            printXyz . getDurationPerDayInWeek x . flip UTCTime (secondsToDiffTime ((60*60*24) - 1)) $ s')

          putStrLn "-----------"
          print "Exported week per day"
        Left e -> error e
    Nothing -> do
      validEntries >>= \case
        Right x -> do
          let x' = getDurationPerDayInWeek x ct
          printXyz $ x'
          print "Exported week per day"
        Left e -> error e
-- work False False (Just dstr) = do
--   ct <- getCurrentTime
--   validEntries >>= \case
--     Right x -> do
--       let timeFromString = parseTimeOrError True defaultTimeLocale "%-d %-m %Y" dstr :: UTCTime
--       let drange = [utctDay timeFromString.. utctDay ct ]
--       forM_ drange (\day -> do
--                        case (sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay day x)) of
--                          0 -> pure ()
--                          v -> printXyz [(day, v)]
--                    )
--       let totalf = fmap (\day -> sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay day x)) drange
--       print "Exported all successful"
--       print "Total:"
--       print $
--         ((fromIntegral $ floor $ sum totalf) / (60 * 60))
--     Left e -> error e

printXyz :: [(Day, NominalDiffTime)] -> IO ()
printXyz x = do
  let f = (,)
            <$> ((show . fst))
            <*> (myFormatDiffTime . snd)
  -- let fff = (,,) <$> (show . fst) <*> y y
  -- grrr wait for time-1.9.2???
  -- let days = [Monday .. Sunday]
  let days = ["M", "T", "W", "T", "F", "S", "S" ]
  forM_ (zip days (f <$> x)) $ \x' -> do
    print x'
