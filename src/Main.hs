{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle (Parser,Text,UTCTime, argText, options,date, testfile, decodeString)
import Data.Aeson
import Data.Time.Format
import Data.Bool

import Common

parser :: Parser (Text, Text)
parser = (,) <$> argText "task"  ""
             <*> argText "description" ""
main :: IO ()
main = startLog


myFormatUtcTime :: UTCTime -> String
myFormatUtcTime = formatTime defaultTimeLocale "%d/%m/%Y %H:%M"

addJsonArrayElementFile :: LogEntry -> String -> IO ()
addJsonArrayElementFile a fp = do
  eitherDecodeFileStrict fp >>= \case
    Right x -> do
      if recordIsStopped $ last x
        then
          encodeLogFile $ x ++ [a]
        else
          do
            print "Last task:"
            print $ last x
            error "Previous task has not been stopped, not able to start new."
    Left e -> error e

startLog :: IO ()
startLog = do
  testfile (decodeString logPath) >>=
    bool (encodeLogFile ([] :: [LogEntry])) (return ())
  (task, description) <- options "" parser
  time' <- date
  addJsonArrayElementFile
    ((task, description, time', Nothing) :: LogEntry) logPath
  print "Started task"
