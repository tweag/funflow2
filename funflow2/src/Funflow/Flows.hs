{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

{-
 - Utilities to hide the kernmantle API by putting together the user effect and the strand type
 -}
module Funflow.Flows
  ( pureFlow,
    ioFlow,
    externalFlow,
    dockerFlow,
  )
where

import Control.Kernmantle.Rope (strand)
import Funflow.Base (Flow)
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig)
import Funflow.Flows.External (ExternalFlow (ExternalFlow), ExternalFlowConfig)
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))

pureFlow :: (i -> o) -> Flow i o
pureFlow f = strand #simple $ Pure f

ioFlow :: (i -> IO o) -> Flow i o
ioFlow f = strand #simple $ IO f

externalFlow :: ExternalFlowConfig i () -> Flow () ()
externalFlow config = strand #external $ ExternalFlow config

dockerFlow :: DockerFlowConfig i () -> Flow () ()
dockerFlow config = strand #docker $ DockerFlow config
