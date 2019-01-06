{-# LANGUAGE OverloadedStrings #-}
module ResumeTask where

import Turtle (date)
import Data.Aeson
import Control.Lens.Tuple
import Control.Lens.Setter

import Common

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
          printResumeTask lr
        else do
          printLastTask lr
          error "Task still running"
    Left e -> error e
