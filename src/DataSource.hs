module DataSource where

import Types

import Data.Time.Clock
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Control.Applicative
import Data.ByteString.Lazy.Char8 (writeFile)

logPath :: FilePath
logPath = "log.json"

eitherDecodeLog :: IO (Either String [LogEntry])
eitherDecodeLog = eitherDecodeFileStrict logPath

validEntries :: IO (Either String [Entry])
validEntries = (fmap filterValidEntries) <$> eitherDecodeLog

allEntries :: IO (Either String [Entry])
allEntries = (liftA2 (++)) <$> getLatestEntryInProgress <*> validEntries

filterValidEntries :: [LogEntry] -> [Entry]
filterValidEntries [] = []
filterValidEntries (x:xs) =
  validLogEntry x ++ filterValidEntries xs
  where
    validLogEntry (a,b,c, Just d) = [Entry a b c d]
    validLogEntry (_,_,_, Nothing) = []

getLatestEntryInProgress :: IO (Either String [Entry])
getLatestEntryInProgress = do
  ct <- getCurrentTime
  x <- eitherDecodeLog
  case x of 
    Right x' ->
      case last x' of
        (a,b,c, Nothing) -> pure . pure $ [Entry a b c ct]
        _ -> pure . pure $ []
    Left e -> pure $ Left e


decodeLog :: IO (Either String [LogEntry])
decodeLog = eitherDecodeFileStrict logPath

encodeLogFile :: [LogEntry] -> IO ()
encodeLogFile v = do
  Data.ByteString.Lazy.Char8.writeFile logPath $ encodePretty v
