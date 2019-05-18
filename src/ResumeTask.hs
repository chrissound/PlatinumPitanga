{-# LANGUAGE OverloadedStrings #-}
module ResumeTask where

import Turtle (date, testfile, decodeString)
import Data.Bool
import Data.Aeson
import Control.Lens.Tuple
import Control.Lens.Setter
import Data.List (nub)
import Control.Monad
import Data.Text.IO (putStrLn)
import Data.Text (Text)
import Control.Applicative
import qualified Data.Attoparsec.Text as AP
-- import qualified Data.Attoparsec.ByteString.Char8 as C8
import Data.String.Conversions
import LogShow
-- import Main (startLog)

import Common
import Types
import Options.Applicative
import DataSource

resumeTask' :: PitangaResume -> IO ()
resumeTask' (PitangaResumeSuggestion x ) = resumeSuggestion x
resumeTask' (PitangaResume False) = resumeTask
resumeTask' (PitangaResume True) = resumeTaskSuggestions

resumeTask :: IO ()
resumeTask = do
  eitherDecodeFileStrict logPath >>= \case
    Right x -> do
      let lr = last x :: LogEntry
      time' <- date
      let nr = (
              set _3 (time') $
              set _4 (Nothing) $ lr
            )
      if recordIsStopped lr
        then do
          addJsonArrayElementFile nr logPath
          printResumeTask
          showLastTaskLog
        else do
          showLastTaskLog
          error "Task still running"
    Left e -> error e

resumeTaskSuggestions :: IO ()
resumeTaskSuggestions = do
  eitherDecodeFileStrict logPath >>= \case
    Right x -> do
      let _ = x :: [LogEntry]
      let lr = nub ((\(a,b,_,_) -> (a,b)) <$> x)
      forM_ lr (\(a,b) -> Data.Text.IO.putStrLn $ mconcat [a, seperator, b])
    Left e -> error e

resumeSuggestion :: String -> IO ()
resumeSuggestion v = do
  case (AP.parseOnly parse' $ cs v :: Either String (Text,Text)) of
    Right x -> startLog x
    Left e -> error e


parse' :: AP.Parser (Text,Text)
parse' = do
  x <- AP.manyTill AP.anyChar (AP.string seperator)
  -- void $ AP.string seperator
  y <- many AP.anyChar
  pure (cs x, cs y)


seperator :: Text
seperator = " -|-|- "

parser :: Parser (PitangaResume)
parser =
  PitangaResumeSuggestion <$>
  (
      strOption
      (  long "accept-suggestion"
      <> help "One line of output from suggestions"
      <> showDefault
      )

  )
  <|>
  PitangaResume <$>
            switch
            (  long "suggestions"
            <> help "Show last tasks "
            <> showDefault
            )



startLog :: (Text,Text) -> IO ()
startLog (t,d) = do
  testfile (decodeString logPath) >>=
    bool (encodeLogFile ([] :: [LogEntry])) (return ())
  -- (t, d) <- options "" parser
  time' <- date
  let nr = (t, d, time', Nothing)
  addJsonArrayElementFile
    (nr) logPath
  printStartedTask
  showLastTaskLog
