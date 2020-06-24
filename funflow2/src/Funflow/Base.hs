{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

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
import Control.External (Env (EnvExplicit), ExternalTask (..), OutputCapture (StdOutCapture), Param, ParamField, TaskDescription (..), contentParam, textParam)
import Control.External.Executor (execute)
import Control.Kernmantle.Caching (ProvidesCaching)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope (AnyRopeWith, Entwines, HasKleisli, HasKleisliIO, LooseRope, SatisfiesAll, liftKleisliIO)
import Control.Kernmantle.Rope ((&), perform, runReader, strand, untwine, weave, weave')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentStore as CS
import Data.String (fromString)
import Data.Text (Text, unpack)
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as D
import Funflow.Flows.External (ExternalFlow (ExternalFlow), ExternalFlowConfig (ExternalFlowConfig))
import qualified Funflow.Flows.External as E
import Funflow.Flows.Nix (NixFlow (NixFlow), NixFlowConfig (NixFlowConfig))
import qualified Funflow.Flows.Nix as N
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Katip (closeScribes, defaultScribeSettings, initLogEnv, registerScribe, runKatipContextT)
import Katip (ColorStrategy (ColorIfTerminal), Severity (InfoS), Verbosity (V2), mkHandleScribe, permitItem)
import Path (Abs, Dir, absdir)
import System.IO (stdout)
import qualified Text.URI as URI

-- The constraints on the set of "strands"
-- These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[  '("simple", SimpleFlow),
      '("external", ExternalFlow),
      '("docker", DockerFlow),
      '("nix", NixFlow)
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
          & weave #docker interpretDockerFlow
          & weave #nix interpretNixFlow
          & weave' #external (interpretExternalFlow store)
          & weave' #simple interpretSimpleFlow
          -- Strip of empty list of strands (after all weaves)
          & untwine
          -- Define the caching
          -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
          & runReader (localStoreWithId store $ defaultCachingId)
          -- Finally, run
          & perform input

-- Interpret simple flow
interpretSimpleFlow :: (Arrow a, HasKleisliIO m a) => SimpleFlow i o -> a i o
interpretSimpleFlow simpleFlow = case simpleFlow of
  Pure f -> arr f
  IO f -> liftKleisliIO f

-- Interpret external flow
interpretExternalFlow :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> ExternalFlow i o -> a i o
interpretExternalFlow store externalFlow = case externalFlow of
  ExternalFlow (ExternalFlowConfig {E.command, E.args, E.env}) -> liftKleisliIO $ \_ -> do
    -- Create the task description (task + cache hash)
    let task :: ExternalTask
        task =
          ExternalTask
            { _etCommand = command,
              -- TODO use input env
              _etEnv = EnvExplicit [(x, (fromString . unpack) y) | (x, y) <- env],
              -- TODO use input args
              _etParams = fmap (fromString . unpack) args,
              _etWriteToStdOut = StdOutCapture
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

-- A type alias to clarify the type of functions that will reinterpret
-- to use for interpretation functions that will be called by `weave`
type WeaverFor name eff strands coreConstraints =
  forall mantle core i o.
  (Entwines (LooseRope mantle core) strands, SatisfiesAll core coreConstraints) =>
  (forall x y. (LooseRope ('(name, eff) ': mantle) core x y -> core x y)) ->
  eff i o ->
  core i o

-- Interpret docker flow
interpretDockerFlow :: WeaverFor "docker" DockerFlow '[ '("external", ExternalFlow)] '[]
interpretDockerFlow reinterpret dockerFlow = case dockerFlow of
  DockerFlow (DockerFlowConfig {D.image, D.command, D.args}) ->
    let externalCommand = "docker"
        externalArgs = "run" : image : command : args
        externalEnv = []
     in reinterpret $ strand #external $ ExternalFlow $ ExternalFlowConfig {E.command = externalCommand, E.args = externalArgs, E.env = externalEnv}

-- Interpret nix flow
interpretNixFlow :: WeaverFor "nix" NixFlow '[ '("external", ExternalFlow)] '[]
interpretNixFlow reinterpret nixFlow = case nixFlow of
  NixFlow (NixFlowConfig {N.nixEnv, N.nixpkgsSource, N.command, N.args, N.env}) ->
    let externalCommand = "nix-shell"
        externalArgs = ("--run" : command : args) ++ packageSpec nixEnv
        externalEnv = ("NIX_PATH", nixpkgsSourceToParam nixpkgsSource) : env
     in reinterpret $ strand #external $ ExternalFlow $ ExternalFlowConfig {E.command = externalCommand, E.args = externalArgs, E.env = externalEnv}
    where
      packageSpec :: N.Environment -> [Text]
      packageSpec (N.ShellFile ip) = [ip]
      packageSpec (N.PackageList ps) = [("-p " <> p) | p <- ps]
      nixpkgsSourceToParam :: N.NixpkgsSource -> Text 
      nixpkgsSourceToParam N.NIX_PATH = "$NIX_PATH"
      nixpkgsSourceToParam (N.NixpkgsTarball uri) = ("nixpkgs=" <> URI.render uri)
