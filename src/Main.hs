{-# LANGUAGE OverloadedStrings #-}
module Main where

import Turtle (Parser,Text,UTCTime, argText, options,date, testfile, decodeString)
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

startLog :: IO ()
startLog = do
  testfile (decodeString logPath) >>=
    bool (encodeLogFile ([] :: [LogEntry])) (return ())
  (t, d) <- options "" parser
  time' <- date
  let nr = (t, d, time', Nothing)
  addJsonArrayElementFile
    (nr) logPath
  printStartedTask nr
