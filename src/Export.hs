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


import Common
import Log

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

main :: IO ()
main = join . customExecParser (prefs showHelpOnError) $
  info (helper <*> parser)
  (  fullDesc
  <> header "list directories"
  )
  where
    parser :: Parser (IO ())
    parser =
      work
        <$> switch
            (  long "this-week"
            <> help ""
            <> showDefault
            )
        <*>
            switch
            (  long "this-week-per-day"
            <> help ""
            <> showDefault
            )
        <*>
            strOption
            (  long "from"
            <> help ""
            <> showDefault
            )

work :: Bool -> Bool -> String -> IO ()
work True _ _  = do
  exportWeek
  print "Exported this week successful"
work _ True _ = do
  ct <- getCurrentTime
  validEntries >>= \case
    Right x -> do
      let x' = getDurationPerDayInWeek x ct
      let x'' = getDurationPerDayInWeek x (addUTCTime (7 * (-nominalDay)) ct)
      printXyz $ x''
      print "Exported previous"
      putStrLn ""
      printXyz $ x'
      print "Exported this week per day"
    Left e -> error e
work False False dstr = do
  ct <- getCurrentTime
  validEntries >>= \case
    Right x -> do
      let timeFromString = parseTimeOrError True defaultTimeLocale "%-d %-m %Y" dstr :: UTCTime
      let drange = [utctDay timeFromString.. utctDay ct ]
      forM_ drange (\day -> do
                       case (sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay day x)) of
                         0 -> pure ()
                         v -> printXyz [(day, v)]
                   )
      let totalf = fmap (\day -> sum $ fmap (diffUTCTime <$> end <*> start) (getDurationForEntriesOnDay day x)) drange
      print "Exported all successful"
      print "Total:"
      print $
        ((fromIntegral $ floor $ sum totalf) / (60 * 60))
    Left e -> error e

printXyz :: (Foldable t) => t (Day, NominalDiffTime) -> IO ()
printXyz x = forM_ x (\(a, b) -> print $ mconcat [show a, " ", myFormatDiffTime b])
