module Types where

-- import Turtle (Parser,Text,UTCTime, argText, date, testfile, decodeString)
import Data.Text
import Data.Time
-- import Data.Time.Format
-- import Data.Bool

-- import Common
-- import Options.Applicative

-- import StopTask
-- import ResumeTask
-- import Log
-- import Export
-- import Amend

data PitangaCommand =
    PitangaStart (Text, Text)
  | PitangaStop
  | PitangaCommandResume PitangaResume
  | PitangaLog (PitangaLogCommand)
  | PitangaExport Export2
  | PitangaAmend PitangaAmendCommand


data Limit = First Int | Last Int
data PitangaLogCommand = PitangaLogCommand (Maybe Limit)

data PitangaResume = PitangaResume Bool | PitangaResumeSuggestion String

data Export = ExportRawJson | ExportGroupByDay | ExportGroupByDayAndTask
data Export2 = Export2 (Export) (Maybe String)


newtype Start = Start DiffTime deriving (Show)
newtype End = End DiffTime deriving (Show)
newtype Duration = Duration DiffTime deriving (Show)

data PitangaAmendCommand =
    PitangaAmendCommand (Maybe Start) (Maybe End) (Maybe Duration)
  | PitangaAmendRestartCommand

type LogEntry = (Text, Text, UTCTime, Maybe (UTCTime))
type WeekEntry = [(Day, NominalDiffTime)]
data Entry = Entry {
    task :: Text
  , description :: Text
  , start :: UTCTime
  , end :: UTCTime
} deriving Show
