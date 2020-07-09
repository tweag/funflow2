{-
 - Entry point to Funflow
 -}
module Funflow
  ( -- Basics
    Flow,
    FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    runFlow,
    -- Caching
    caching,
    -- Helpers to make flows in an idiomatic way
    pureFlow,
    ioFlow,
    shellFlow,
    commandFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Caching (caching)
import Funflow.Base (CommandExecutionHandler (..), Flow, FlowExecutionConfig (..), runFlow)
import Funflow.Flows
  ( commandFlow,
    dockerFlow,
    ioFlow,
    nixFlow,
    pureFlow,
    shellFlow,
  )
