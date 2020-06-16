{-
 - Entry point to Funflow
 -}
module Funflow
  ( Flow,
    runFlow,
    cached,
    cachedWithProps,
    cachedIO,
    cachedIOWithProps,
  )
where

import Funflow.Base (Flow, runFlow)
import Funflow.Flows (cached, cachedIO, cachedIOWithProps, cachedWithProps)
