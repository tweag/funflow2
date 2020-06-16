{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Funflow where

import Control.Arrow (Arrow)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli)
import Control.Monad.IO.Class (MonadIO)
import Funflow.Flows.Cached (CachedFlow)

-- TODO
-- import Funflow.Flows.External (ExternalFlow)
-- import Funflow.Flows.Store (DirectStoreAccessFlow)

-- The constraints on the set of user effects (strands).
-- These will be "interpreted" to the CoreEffect defined below
type RequiredStrands =
  '[  '("cached", CachedFlow)
      --'("externalStep", ExternalFlow),
      --'("directStoreAccess", DirectStoreAccessFlow)
   ]

-- The constraints on the core effect (binary effect).
-- The associated class instances will be used to interpret
-- the flow once the user effects have been interpreted.
type RequiredCoreEffects m = '[Arrow, ProvidesCaching, HasKleisli m]

-- The main type: a Flow is a task that takes type `input` and produces type `output`
type Flow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCoreEffects m)
    input
    output
