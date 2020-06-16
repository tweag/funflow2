{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

{-
 - Utilities to hide the kernmantle API by putting together the user effect and the strand type
 -}
module Funflow.Flows
  ( pureFlowWithProps,
    pureFlow,
    ioFlowWithProps,
    ioFlow,
  )
where

import Control.Kernmantle.Rope (strand)
import Data.Default (def)
import Funflow.Base (Flow)
import Funflow.Flows.Simple (SimpleFlow (IO, Pure), SimpleFlowProperties)

pureFlowWithProps :: SimpleFlowProperties i o -> (i -> o) -> Flow i o
pureFlowWithProps props f = strand #simple $ Pure props f

pureFlow :: (i -> o) -> Flow i o
pureFlow f = pureFlowWithProps def f

ioFlowWithProps :: SimpleFlowProperties i o -> (i -> IO o) -> Flow i o
ioFlowWithProps props f = strand #simple $ IO props f

ioFlow :: (i -> IO o) -> Flow i o
ioFlow f = ioFlowWithProps def f
