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

work :: Bool -> Bool -> IO ()
work True _  = do
  exportWeek
  print "Exported this week successful"
work _ True = do
  ct <- getCurrentTime
  validEntries >>= \case
    Right x -> do
      let x' = getDurationPerDayInWeek x ct
      let x'' = getDurationPerDayInWeek x (addUTCTime ((-nominalDay)) ct)
      pPrint $ fmap (\(a,b) -> (a, myFormatDiffTime b)) x''
      print "Exported previous"
      pPrint $ fmap (\(a,b) -> (a, myFormatDiffTime b)) x'
      print "Exported this week per day"
    Left e -> error e
work False False = do
  export
  print "Exported all successful"
