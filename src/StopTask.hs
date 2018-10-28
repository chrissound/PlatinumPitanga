module StopTask where

import Turtle (date)
import Control.Lens.Tuple
import Control.Lens.Setter
import Rainbow

import Common

main :: IO ()
main = stopLog

stopLog :: IO ()
stopLog = do
  time' <- date
  decodeLog >>= \case
    Right ls -> do
      let lr = last ls
      case recordIsStopped lr of
        True -> do
          putChunkLn $ chunk "No task has been started, not able to stop." & fore red
          print "Last task:"
          print $ lr
          error ""
        False -> do
          encodeLogFile $
            fmapAtIndex (length ls - 1) (set _4 (Just time')) ls
          printStoppedTask lr
    Left e -> error e
