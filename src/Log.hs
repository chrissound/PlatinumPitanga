{-# LANGUAGE OverloadedStrings #-}
module Log where

import Control.Monad
import Data.String.Conversions
import Data.Time.Clock
import Common

showLog :: IO ()
showLog = do
  eitherDecodeLog >>= \case
    Right x -> do
      forM_ (reverse x) $ \(t,d,s,e) -> do
        case e of
          Just e' -> do
            putStrLn $ mconcat [myFormatUtcTime s, " - ", myFormatUtcTime e']
            putStrLn $ myFormatDiffTime $ diffUTCTime e' s
          Nothing -> putStrLn $ mconcat [myFormatUtcTime s, " - ---"]
        putStrLn $ cs $ mconcat [t, " - ", d, "\n"]
    Left e -> error e
