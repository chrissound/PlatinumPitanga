module StopTask where

import Turtle (date)
import Control.Lens.Tuple
import Control.Lens.Setter

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
          print "Last task:"
          print $ lr
          error "No task has been started, not able to stop."
        False -> do
          encodeLogFile $
            fmapAtIndex (length ls - 1) (set _4 (Just time')) ls
          print "Last task:"
          print $ lr
          print "Stopped task"
    Left e -> error e
