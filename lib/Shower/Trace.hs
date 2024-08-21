-- | Wrappers for Debug.Trace functions with a nicer layout.

module Shower.Trace
  ( traceShower,
    traceShowerId,
    traceShowerWith,
    traceShowerM
  ) where

import Debug.Trace
import Shower (shower)

traceShower :: Show a => a -> b -> b
traceShower = trace . shower

traceShowerId :: Show a => a -> a
traceShowerId x = trace (shower x) x

traceShowerWith :: Show b => (a -> b) -> a -> a
traceShowerWith f x = trace (shower $ f x) x

traceShowerM :: (Show a, Monad m) => a -> m ()
traceShowerM = traceM . shower
