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
    FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    runFlow,
  )
where

import Control.Arrow (Arrow, arr)
import Control.Exception (bracket)
import Control.External (Env (EnvExplicit), ExternalTask (..), OutputCapture (StdOutCapture), TaskDescription (..))
import Control.External.Executor (execute)
import Control.Kernmantle.Caching (ProvidesCaching, localStoreWithId)
import Control.Kernmantle.Rope (AnyRopeWith, Entwines, HasKleisli, HasKleisliIO, LooseRope, SatisfiesAll, liftKleisliIO, perform, runReader, strand, untwine, weave, weave', (&))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentStore as CS
import Data.String (fromString)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Funflow.Flows.Command (CommandFlow (CommandFlow, ShellCommandFlow), CommandFlowConfig (CommandFlowConfig))
import qualified Funflow.Flows.Command as CF
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as DF
import Funflow.Flows.Nix (NixFlow (NixFlow), NixFlowConfig (NixFlowConfig))
import qualified Funflow.Flows.Nix as NF
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Katip (ColorStrategy (ColorIfTerminal), Severity (InfoS), Verbosity (V2), closeScribes, defaultScribeSettings, initLogEnv, mkHandleScribe, permitItem, registerScribe, runKatipContextT)
import Path (Abs, Dir, absdir)
import System.IO (stdout)
import System.Process (callCommand)
import qualified Text.URI as URI

-- The constraints on the set of "strands"
-- These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[ '("simple", SimpleFlow),
     '("command", CommandFlow),
     '("docker", DockerFlow),
     '("nix", NixFlow)
   ]

-- The class constraints on the "core effect".
-- The "core effect" is the effect used to run any kind of "binary effect" ("strand")
type RequiredCoreEffects m =
  '[ -- Basic requirement
     Arrow,
     -- Support IO
     HasKleisli m,
     -- Support caching
     ProvidesCaching
   ]

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
data FlowExecutionConfig = FlowExecutionConfig
  { commandExecution :: CommandExecutionHandler
  -- , executionEnvironment :: CommandExecutionEnvironment
  }

data CommandExecutionHandler = SystemExecutor | ExternalExecutor

-- data CommandHashStrategy = Smart | Rigorous
-- data CommandExecutionEnvironment = SystemEnvironment | Nix | Docker

runFlow :: FlowExecutionConfig -> Flow input output -> input -> IO output
runFlow (FlowExecutionConfig {commandExecution}) flow input =
  let -- TODO choose path
      defaultPath = [absdir|/tmp/funflow/store|]
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore defaultPath $ \store -> do
        flow
          -- Weave effects
          & weave #docker interpretDockerFlow
          & weave #nix interpretNixFlow
          & weave'
            #command
            ( case commandExecution of
                SystemExecutor -> interpretCommandFlowVanilla
                -- change
                ExternalExecutor -> interpretCommandFlowExternalExecutor store
            )
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

-- Possible interpreters for the command flow
interpretCommandFlowVanilla :: (Arrow a, HasKleisliIO m a) => CommandFlow i o -> a i o
interpretCommandFlowVanilla commandFlow = case commandFlow of
  ShellCommandFlow shellCommand -> liftKleisliIO $
    \() -> callCommand $ T.unpack shellCommand
  -- TODO
  CommandFlow _ -> liftKleisliIO $ \() -> return ()

interpretCommandFlowExternalExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowExternalExecutor store commandFlow = case commandFlow of
  CommandFlow (CommandFlowConfig {CF.command, CF.args, CF.env}) -> liftKleisliIO $ \_ -> do
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
    -- Finish
    return ()
  -- TODO
  ShellCommandFlow _ -> liftKleisliIO $ \() -> return ()

-- A type alias to clarify the type of functions that will reinterpret
-- to use for interpretation functions that will be called by `weave`
type WeaverFor name eff strands coreConstraints =
  forall mantle core i o.
  (Entwines (LooseRope mantle core) strands, SatisfiesAll core coreConstraints) =>
  (forall x y. (LooseRope ('(name, eff) ': mantle) core x y -> core x y)) ->
  eff i o ->
  core i o

-- Interpret docker flow
interpretDockerFlow :: WeaverFor "docker" DockerFlow '[ '("command", CommandFlow)] '[]
interpretDockerFlow reinterpret dockerFlow = case dockerFlow of
  DockerFlow (DockerFlowConfig {DF.image, DF.command, DF.args}) ->
    let executorCommand = "docker"
        executorArgs = "run" : image : command : args
        executorEnv = []
     in reinterpret $ strand #command $ CommandFlow $ CommandFlowConfig {CF.command = executorCommand, CF.args = executorArgs, CF.env = executorEnv}

-- Interpret nix flow
interpretNixFlow :: WeaverFor "nix" NixFlow '[ '("command", CommandFlow)] '[]
interpretNixFlow reinterpret nixFlow = case nixFlow of
  NixFlow (NixFlowConfig {NF.nixEnv, NF.nixpkgsSource, NF.command, NF.args, NF.env}) ->
    let executorCommand = "nix-shell"
        executorArgs = ("--run" : command : args) ++ packageSpec nixEnv
        executorEnv = ("NIX_PATH", nixpkgsSourceToParam nixpkgsSource) : env
     in reinterpret $ strand #command $ CommandFlow $ CommandFlowConfig {CF.command = executorCommand, CF.args = executorArgs, CF.env = executorEnv}
    where
      packageSpec :: NF.Environment -> [Text]
      packageSpec (NF.ShellFile ip) = [ip]
      packageSpec (NF.PackageList ps) = [("-p " <> p) | p <- ps]
      nixpkgsSourceToParam :: NF.NixpkgsSource -> Text
      nixpkgsSourceToParam NF.NIX_PATH = "$NIX_PATH"
      nixpkgsSourceToParam (NF.NixpkgsTarball uri) = ("nixpkgs=" <> URI.render uri)
