module Common where

import Turtle (UTCTime, Text)
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 (writeFile)
import Data.Maybe

type LogEntry = (Text, Text, UTCTime, Maybe (UTCTime))

decodeLog :: IO (Either String [LogEntry])
decodeLog = eitherDecodeFileStrict logPath

fmapAtIndex :: Int -> (a -> a) -> [a] -> [a]
fmapAtIndex n f ls = a ++ (f item:b)
  where (a, (item:b)) = splitAt n ls

logPath :: FilePath
logPath = "log.json"

encodeLogFile :: ToJSON a => a -> IO ()
encodeLogFile v = do
  Data.ByteString.Lazy.Char8.writeFile logPath $ encodePretty v

recordIsStopped :: (a,b,c, Maybe d) -> Bool
recordIsStopped (_,_,_,endTime) = isJust endTime
