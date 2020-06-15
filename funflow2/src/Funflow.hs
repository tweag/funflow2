{-# LANGUAGE DataKinds #-}

module Funflow where

import Control.Kernmantle.Rope (TightRope)
import Funflow.Flows.Cached (CachedFlow, CachedFlowProperties, MetadataWriter)
import Funflow.Flows.External (ExternalFlow)
import Funflow.Flows.Store (DirectStoreAccessFlow)

type FunflowStrands =
  '[  '("externalStep", ExternalFlow),
      '("cachedStep", CachedFlow),
      '("directStoreAccess", DirectStoreAccessFlow)
   ]

type Flow i o = TightRope
