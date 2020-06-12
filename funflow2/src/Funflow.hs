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
    shellFlow,
    executorFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Caching (caching)
import Funflow.Base (Flow, runFlow)
import Funflow.Flows
  ( dockerFlow,
    executorFlow,
    ioFlow,
    nixFlow,
    pureFlow,
    shellFlow,
  )
