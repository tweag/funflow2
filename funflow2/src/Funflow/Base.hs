{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Funflow.Base
  ( Flow,
    RequiredStrands,
    RequiredCoreEffects,
    runFlow,
    interpretSimpleFlow,
    interpretExternalFlow,
  )
where

import Control.Arrow (Arrow)
import Control.Arrow (arr)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, SieveTrans, liftKleisliIO)
import Control.Kernmantle.Rope ((&), perform, runReader, untwine, weave')
import Control.Monad.IO.Class (MonadIO)
import qualified Data.CAS.ContentStore as CS
import Funflow.Flows.External (ExternalFlow (RunCommand))
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Path (Abs, Dir, absdir)

-- The constraints on the set of "strands"
-- These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[  '("simple", SimpleFlow),
      '("external", ExternalFlow)
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
runFlow flow input =
  -- Run with store to enable caching (with default path to store)
  CS.withStore [absdir|/tmp/funflow/store|] $ \store -> do
    flow
      -- Weave effects
      & weave' #simple interpretSimpleFlow
      & weave' #external interpretExternalFlow
      -- Strip of empty list of strands (after all weaves)
      & untwine
      -- Define the caching
      -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
      & runReader (localStoreWithId store $ Just 1)
      -- Finally, run
      & perform input

-- Interpret simple flow
interpretSimpleFlow :: (Arrow a, SieveTrans m a, MonadIO m) => SimpleFlow i o -> a i o
interpretSimpleFlow simpleFlow = case simpleFlow of
  Pure _ f -> arr f
  IO _ f -> liftKleisliIO f

interpretExternalFlow :: (Arrow a, SieveTrans m a, MonadIO m) => ExternalFlow i o -> a i o
interpretExternalFlow externalFlow = case externalFlow of
  -- TODO
  RunCommand _ _ -> arr $ return ()
