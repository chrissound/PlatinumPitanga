module Types where

-- import Turtle (Parser,Text,UTCTime, argText, date, testfile, decodeString)
import Data.Text
-- import Data.Time.Format
-- import Data.Bool

-- import Common
-- import Options.Applicative

-- import StopTask
import ResumeTask
import Log
import Export

data PitangaCommand = PitangaStart (Text, Text) | PitangaStop | PitangaCommandResume PitangaResume | PitangaLog (PitangaLogCommand) | PitangaExport Export2
