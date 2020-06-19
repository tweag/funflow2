{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Control.Exception (bracket)
import Control.External (Env (EnvExplicit), ExternalTask (..), OutputCapture (NoOutputCapture), TaskDescription (..))
import Control.External.Executor (execute)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, SieveTrans, liftKleisliIO)
import Control.Kernmantle.Rope ((&), perform, runReader, untwine, weave')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentHashable ()
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentStore as CS
import Data.String (fromString)
import Funflow.Flows.External (ExternalFlow (ExternalFlow), ExternalFlowConfig (ExternalFlowConfig), command)
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Katip (closeScribes, defaultScribeSettings, initLogEnv, registerScribe, runKatipContextT)
import Katip (ColorStrategy (ColorIfTerminal), Severity (InfoS), Verbosity (V2), mkHandleScribe, permitItem)
import Path (Abs, Dir, absdir)
import System.IO (stdout)

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
  let -- TODO choose path
      defaultPath = [absdir|/tmp/funflow/store|]
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore defaultPath $ \store -> do
        flow
          -- Weave effects
          & weave' #simple interpretSimpleFlow
          & weave' #external (interpretExternalFlow store)
          -- Strip of empty list of strands (after all weaves)
          & untwine
          -- Define the caching
          -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
          & runReader (localStoreWithId store $ defaultCachingId)
          -- Finally, run
          & perform input

-- Interpret simple flow
interpretSimpleFlow :: (Arrow a, SieveTrans m a, MonadIO m) => SimpleFlow i o -> a i o
interpretSimpleFlow simpleFlow = case simpleFlow of
  Pure f -> arr f
  IO f -> liftKleisliIO f

-- Interpret external flow
interpretExternalFlow :: (Arrow a, SieveTrans m a, MonadIO m) => CS.ContentStore -> ExternalFlow i o -> a i o
interpretExternalFlow store externalFlow = case externalFlow of
  ExternalFlow (ExternalFlowConfig {command}) -> liftKleisliIO $ \_ -> do
    -- Create the task description (task + cache hash)
    let task :: ExternalTask
        task =
          ExternalTask
            { _etCommand = fromString command,
              -- TODO use input env
              _etEnv = EnvExplicit [],
              -- TODO use input args
              _etParams = [],
              _etWriteToStdOut = NoOutputCapture
            }
    hash <- liftIO $ contentHash task
    let taskDescription =
          TaskDescription
            { _tdOutput = hash,
              _tdTask = task
            }
    -- Katip machinery
    handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
    let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "external"
    bracket makeLogEnv closeScribes $ \le -> do
      let initialContext = ()
      let initialNamespace = "funflow"
      runKatipContextT le initialContext initialNamespace $ execute store taskDescription
    -- Done
    return ()
