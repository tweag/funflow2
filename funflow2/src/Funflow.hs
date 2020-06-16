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
    pureFlowWithProps,
    pureFlow,
    ioFlowWithProps,
    ioFlow,
  )
where

import Control.Kernmantle.Caching (caching)
import Funflow.Base (Flow, runFlow)
import Funflow.Flows
  ( ioFlow,
    ioFlowWithProps,
    pureFlow,
    pureFlowWithProps,
  )
