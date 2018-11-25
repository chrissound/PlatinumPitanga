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

import Common
import Log

export :: IO ()
export = do
  validEntries >>= \case
    Right x -> do
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
    Left e -> error e
