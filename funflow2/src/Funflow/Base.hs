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
    interpretExecutorFlow,
  )
where

import Control.Arrow (Arrow)
import Control.Arrow (arr)
import Control.Exception (bracket)
import Control.External (Env (EnvExplicit), ExternalTask (..), OutputCapture (StdOutCapture), TaskDescription (..))
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
import qualified Data.Text as T
import Data.Text (Text, unpack)
import Funflow.Flows.Command (CommandFlow (ShellCommandFlow))
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as D
import Funflow.Flows.Executor (ExecutorFlow (ExecutorFlow), ExecutorFlowConfig (ExecutorFlowConfig))
import qualified Funflow.Flows.Executor as E
import Funflow.Flows.Nix (NixFlow (NixFlow), NixFlowConfig (NixFlowConfig))
import qualified Funflow.Flows.Nix as N
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Katip (closeScribes, defaultScribeSettings, initLogEnv, registerScribe, runKatipContextT)
import Katip (ColorStrategy (ColorIfTerminal), Severity (InfoS), Verbosity (V2), mkHandleScribe, permitItem)
import Path (Abs, Dir, absdir)
import System.IO (stdout)
import System.Process (callCommand)
import qualified Text.URI as URI

-- The constraints on the set of "strands"
-- These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[ '("simple", SimpleFlow),
     '("command", CommandFlow),
     '("executor", ExecutorFlow),
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
          & weave' #executor (interpretExecutorFlow store)
          & weave' #command interpretCommandFlow
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

-- Interpret command flow
interpretCommandFlow :: (Arrow a, HasKleisliIO m a) => CommandFlow i o -> a i o
interpretCommandFlow commandFlow = case commandFlow of
  ShellCommandFlow shellCommand -> liftKleisliIO $
    \() -> callCommand $ T.unpack shellCommand

-- Interpret executor flow
interpretExecutorFlow :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> ExecutorFlow i o -> a i o
interpretExecutorFlow store executorFlow = case executorFlow of
  ExecutorFlow (ExecutorFlowConfig {E.command, E.args, E.env}) -> liftKleisliIO $ \_ -> do
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
    let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "executor"
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
interpretDockerFlow :: WeaverFor "docker" DockerFlow '[ '("executor", ExecutorFlow)] '[]
interpretDockerFlow reinterpret dockerFlow = case dockerFlow of
  DockerFlow (DockerFlowConfig {D.image, D.command, D.args}) ->
    let executorCommand = "docker"
        executorArgs = "run" : image : command : args
        executorEnv = []
     in reinterpret $ strand #executor $ ExecutorFlow $ ExecutorFlowConfig {E.command = executorCommand, E.args = executorArgs, E.env = executorEnv}

-- Interpret nix flow
interpretNixFlow :: WeaverFor "nix" NixFlow '[ '("executor", ExecutorFlow)] '[]
interpretNixFlow reinterpret nixFlow = case nixFlow of
  NixFlow (NixFlowConfig {N.nixEnv, N.nixpkgsSource, N.command, N.args, N.env}) ->
    let executorCommand = "nix-shell"
        executorArgs = ("--run" : command : args) ++ packageSpec nixEnv
        executorEnv = ("NIX_PATH", nixpkgsSourceToParam nixpkgsSource) : env
     in reinterpret $ strand #executor $ ExecutorFlow $ ExecutorFlowConfig {E.command = executorCommand, E.args = executorArgs, E.env = executorEnv}
    where
      packageSpec :: N.Environment -> [Text]
      packageSpec (N.ShellFile ip) = [ip]
      packageSpec (N.PackageList ps) = [("-p " <> p) | p <- ps]
      nixpkgsSourceToParam :: N.NixpkgsSource -> Text
      nixpkgsSourceToParam N.NIX_PATH = "$NIX_PATH"
      nixpkgsSourceToParam (N.NixpkgsTarball uri) = ("nixpkgs=" <> URI.render uri)
