module Current where

import Data.Time
import DataSource
-- import Common

printCurrentInt :: IO ()
printCurrentInt = do
  ct <- getCurrentTime
  decodeLog >>= \case
    Right ls -> do
      let lr = last ls
      case lr of
        (_,_,_, Just _) -> putStrLn "-1"
        (_,_,s, Nothing)-> print $ floor $ ((diffUTCTime ct s) / (60 * 60)) * 4

    Left e -> error e
