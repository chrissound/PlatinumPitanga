{-# OPTIONS -Wno-unused-imports #-}
module MyDebug where

import Debug.Trace

myTraceShow :: Show a => String -> a -> a
myTraceShow s = traceShow
  <$> ((++) (s ++ ": ") . show)
  <*> id
 -- myTraceShow _ = id
