{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle (Parser,Text,UTCTime, argText, date, testfile, decodeString)
import Data.Time.Format
import Data.Bool

import Common
import Options.Applicative

import StopTask
import ResumeTask
import Log
import Export (work, Export2(..), parser)

data PitangaCommand = PitangaStart (Text, Text) | PitangaStop | PitangaResume | PitangaLog (PitangaLogCommand) | PitangaExport Export2

parser :: Parser PitangaCommand --(Text, Text)
parser =
  subparser
    (command "start"
        (info
          ((
            PitangaStart <$>
            (
              (,)
              <$> argText "task"  ""
              <*> argText "description" ""
            )
          ) <**> helper)
          $ progDesc "Start a new task"
        )
      )
  <|>
  subparser
    (command "stop"
        (info
          (pure PitangaStop <**> helper)
          $ progDesc "Stop the task that is currently in progress"
        )
      )
  <|>
  subparser
    (command "resume"
        (info
          (pure PitangaResume <**> helper)
          $ progDesc "Resume the last stopped task"
        )
      )
  <|>
  subparser
    (command "log"
        (info
          ((PitangaLog <$> Log.parser) <**> helper)
          $ progDesc "Log"
        )
      )
  <|>
  subparser
    (command "export"
        (info
          ((PitangaExport <$> Export.parser) <**> helper)
          $ progDesc "Export"
        )
      )

main :: IO ()
main = pitangaCommand =<< execParser opts
  where
    opts = info (Main.parser <**> helper)
      (
         fullDesc
      <> progDesc ""
      <> header "PlatinumPitanga"
      )

myFormatUtcTime :: UTCTime -> String
myFormatUtcTime = formatTime defaultTimeLocale "%d/%m/%Y %H:%M"

pitangaCommand :: PitangaCommand -> IO ()
pitangaCommand (PitangaStart x) = startLog x
pitangaCommand (PitangaStop) = stopLog
pitangaCommand (PitangaResume) = resumeTask
pitangaCommand (PitangaLog x) = showLog x
pitangaCommand (PitangaExport x) = work x

startLog :: (Text,Text) -> IO ()
startLog (t,d) = do
  testfile (decodeString logPath) >>=
    bool (encodeLogFile ([] :: [LogEntry])) (return ())
  -- (t, d) <- options "" parser
  time' <- date
  let nr = (t, d, time', Nothing)
  addJsonArrayElementFile
    (nr) logPath
  printStartedTask nr
