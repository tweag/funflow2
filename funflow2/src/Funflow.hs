{-
 - Entry point to Funflow
 -}
module Funflow
  ( -- Basics
    Flow,
    runFlow,
    -- Caching
    caching,
    -- Helpers to make flows in an idiomatic way
    pureFlow,
    ioFlow,
    externalFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Caching (caching)
import Funflow.Base (Flow, runFlow)
import Funflow.Flows
  ( dockerFlow,
    nixFlow,
    externalFlow,
    ioFlow,
    pureFlow,
  )
