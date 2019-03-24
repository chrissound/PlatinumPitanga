{-# OPTIONS -Wno-unused-imports #-}
module MyDebug where

import Debug.Trace

myd :: Show a => String -> a -> a
myd s = traceShow
  <$> ((++) (s ++ ": ") . show)
  <*> id
 -- myTraceShow _ = id
