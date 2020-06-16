{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Funflow.Base
  ( Flow,
    RequiredStrands,
    RequiredCoreEffects,
    runFlow,
  )
where

import Control.Arrow (Arrow)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli)
import Control.Kernmantle.Rope ((&), perform, runReader, untwine, weave')
import Control.Monad.IO.Class (MonadIO)
import qualified Data.CAS.ContentStore as CS
import Funflow.Flows.Cached (CachedFlow, runCached)
import Path (Abs, Dir, absdir)

-- The constraints on the set of "user effects" ("strands").
-- These will be "interpreted" into "core effects" which have contraints defined below.
type RequiredStrands =
  '[  '("cached", CachedFlow)
      --'("externalStep", ExternalFlow),
      --'("directStoreAccess", DirectStoreAccessFlow)
   ]

-- The class constraints on the "core effect".
-- The "core effect" is the effect used to run any kind of "user effect" ("strand")
type RequiredCoreEffects m = '[Arrow, ProvidesCaching, HasKleisli m]

-- Flow is the main type of Funflow.
-- It is a task that takes an input of type `input` and produces an output of type `output`.
-- It can use any "user effect" ("strand") that is defined in the required strands above.
type Flow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCoreEffects m)
    input
    output

-- Run a flow
runFlow :: Flow input output -> input -> IO output
runFlow flow input = CS.withStore [absdir|/tmp/_store|] $ \store -> do
  flow
    & weave' #cached runCached
    & untwine
    & runReader (localStoreWithId store $ Just 1)
    & perform input
