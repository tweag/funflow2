{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Funflow.Run
  ( FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    defaultExecutionConfig,
    runFlow,
  )
where

import Control.Arrow (Arrow, arr)
import Control.Exception (bracket)
import Control.External
  ( Env (EnvExplicit),
    ExternalTask (..),
    OutputCapture (StdOutCapture),
    TaskDescription (..),
  )
import Control.External.Executor (execute)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope
  ( Entwines,
    HasKleisliIO,
    LooseRope,
    SatisfiesAll,
    liftKleisliIO,
    perform,
    runReader,
    strand,
    untwine,
    weave,
    weave',
    (&),
  )
import Control.Monad.IO.Class (liftIO)
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.String (fromString)
import Data.Text (Text, unpack)
import qualified Data.Text as T
import Funflow.Flow (Flow)
import Funflow.Flows.Command
  ( CommandFlow (CommandFlow, ShellCommandFlow, WritingCommandFlow),
    CommandFlowConfig (CommandFlowConfig),
    WritingCommandFlowInput (WritingCommandFlowInput),
  )
import qualified Funflow.Flows.Command as CF
import Funflow.Flows.Docker
  ( DockerFlow (DockerFlow),
    DockerFlowConfig (DockerFlowConfig),
  )
import qualified Funflow.Flows.Docker as DF
import Funflow.Flows.Nix
  ( NixFlow (NixFlow),
    NixFlowConfig (NixFlowConfig),
  )
import qualified Funflow.Flows.Nix as NF
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))
import Katip
  ( ColorStrategy (ColorIfTerminal),
    Severity (InfoS),
    Verbosity (V2),
    closeScribes,
    defaultScribeSettings,
    initLogEnv,
    mkHandleScribe,
    permitItem,
    registerScribe,
    runKatipContextT,
  )
import Path (Abs, Dir, absdir, fromAbsDir)
import Path.IO (copyDirRecur)
import System.Environment (getEnv)
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
  ( CmdSpec (RawCommand, ShellCommand),
    CreateProcess (CreateProcess),
    StdStream (Inherit),
    child_group,
    child_user,
    close_fds,
    cmdspec,
    createProcess,
    create_group,
    create_new_console,
    cwd,
    delegate_ctlc,
    detach_console,
    env,
    new_session,
    std_err,
    std_in,
    std_out,
    use_process_jobs,
  )
import qualified Text.URI as URI

-- Run a flow
data FlowExecutionConfig = FlowExecutionConfig
  { commandExecution :: CommandExecutionHandler
  -- , executionEnvironment :: CommandExecutionEnvironment
  }

data CommandExecutionHandler = SystemExecutor | ExternalExecutor

-- data CommandHashStrategy = Smart | Rigorous
-- data CommandExecutionEnvironment = SystemEnvironment | Nix | Docker

defaultExecutionConfig :: FlowExecutionConfig
defaultExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}

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
                SystemExecutor -> interpretCommandFlowSystemExecutor store
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

-- System executor: just spawn processes
interpretCommandFlowSystemExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowSystemExecutor store commandFlow =
  case commandFlow of
    CommandFlow (CommandFlowConfig {CF.command, CF.args, CF.env}) ->
      liftKleisliIO $ \() ->
        do
          _ <-
            createProcess $
              CreateProcess
                { cmdspec = RawCommand (T.unpack command) (fmap T.unpack args),
                  env = Nothing, -- TODO change to `Just $ mapPair T.unpack env`
                  cwd = Nothing,
                  std_in = Inherit,
                  std_out = Inherit,
                  std_err = Inherit,
                  close_fds = False,
                  create_group = False,
                  delegate_ctlc = False,
                  detach_console = False,
                  create_new_console = False,
                  new_session = False,
                  child_group = Nothing,
                  child_user = Nothing,
                  use_process_jobs = False
                }
          return ()
    ShellCommandFlow shellCommand ->
      liftKleisliIO $ \() ->
        do
          _ <-
            createProcess $
              CreateProcess
                { cmdspec = ShellCommand (T.unpack shellCommand),
                  env = Nothing, -- TODO change to `Just $ mapPair T.unpack env`
                  cwd = Nothing,
                  std_in = Inherit,
                  std_out = Inherit,
                  std_err = Inherit,
                  close_fds = False,
                  create_group = False,
                  delegate_ctlc = False,
                  detach_console = False,
                  create_new_console = False,
                  new_session = False,
                  child_group = Nothing,
                  child_user = Nothing,
                  use_process_jobs = False
                }
          return ()
    -- In this interpreter, we copy the content of the input CS.Item to a new CS.Item and
    -- use it as the current working directory for the command
    WritingCommandFlow (CommandFlowConfig {CF.command, CF.args}) ->
      liftKleisliIO $ \(WritingCommandFlowInput {CF.workingDirectoryContent}) ->
        do
          -- Make a content store item
          itemHash <- CH.contentHash (command, args, workingDirectoryContent)
          CS.Complete (_, completedItem) <- CS.withConstructIfMissing store RC.NoCache mempty itemHash $ \outputContentItemFilePath ->
            do
              -- Copy files from input
              let inputDirectoryFilePath = CS.itemPath store workingDirectoryContent
              copyDirRecur inputDirectoryFilePath outputContentItemFilePath
              -- Run task with cwd
              _ <-
                createProcess $
                  CreateProcess
                    { cmdspec = RawCommand (T.unpack command) (fmap T.unpack args),
                      env = Nothing, -- TODO change to `Just $ mapPair T.unpack env`
                      cwd = Just $ fromAbsDir outputContentItemFilePath,
                      std_in = Inherit,
                      std_out = Inherit,
                      std_err = Inherit,
                      close_fds = False,
                      create_group = False,
                      delegate_ctlc = False,
                      detach_console = False,
                      create_new_console = False,
                      new_session = False,
                      child_group = Nothing,
                      child_user = Nothing,
                      use_process_jobs = False
                    }
              -- TODO handle failure
              return $ Right ()
          -- Return written CS Item
          return completedItem

-- External executor: use the external-executor package
-- TODO currently little to no benefits, need to allow setting SQL, Redis, etc
interpretCommandFlowExternalExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowExternalExecutor store commandFlow =
  let runTask :: (Arrow a, HasKleisliIO m a) => ExternalTask -> a i ()
      runTask task = liftKleisliIO $ \_ -> do
        -- Hash computation, then bundle it with the task
        hash <- liftIO $ contentHash task
        let taskDescription =
              TaskDescription
                { _tdOutput = hash,
                  _tdTask = task
                }
        -- Katip machinery
        handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V2
        let makeLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "funflow" "executor"
        _ <- bracket makeLogEnv closeScribes $ \logEnv -> do
          let initialContext = ()
          let initialNamespace = "funflow"
          runKatipContextT logEnv initialContext initialNamespace $ execute store taskDescription
        -- Finish
        return ()
   in case commandFlow of
        CommandFlow (CommandFlowConfig {CF.command, CF.args, CF.env}) ->
          -- Create the task description (task + cache hash)
          let task :: ExternalTask
              task =
                ExternalTask
                  { _etCommand = command,
                    _etEnv = EnvExplicit [(x, (fromString . unpack) y) | (x, y) <- env],
                    _etParams = fmap (fromString . unpack) args,
                    _etWriteToStdOut = StdOutCapture
                  }
           in runTask task
        ShellCommandFlow shellCommand ->
          let task =
                ExternalTask
                  { _etCommand = shellCommand,
                    _etEnv = EnvExplicit [],
                    _etParams = [],
                    _etWriteToStdOut = StdOutCapture
                  }
           in runTask task
        WritingCommandFlow _ ->
          -- TODO implement interpreter for WritingCommandFlow
          error "Not implemented"

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
    let commandConfig =
          CommandFlowConfig
            { CF.command = "docker",
              CF.args = "run" : image : command : args,
              CF.env = []
            }
     in reinterpret $ strand #command $ CommandFlow $ commandConfig

-- Interpret nix flow
interpretNixFlow :: WeaverFor "nix" NixFlow '[ '("command", CommandFlow)] '[]
interpretNixFlow reinterpret nixFlow =
  let -- Turn either a Nix file or a set of packages into the right list of arguments for `nix-shell`
      packageSpec :: NF.Environment -> [Text]
      packageSpec (NF.ShellFile shellFile) = [shellFile]
      packageSpec (NF.PackageList packageNames) = [("-p " <> packageName) | packageName <- packageNames]
      -- Turn a NIX_PATH or an URI to a tarball into the right list of arguments for `nix-shell`
      nixpkgsSourceToParam :: NF.NixpkgsSource -> Text
      -- FIXME This is DIRTY as HELL
      nixpkgsSourceToParam NF.NIX_PATH =
        T.pack $ unsafePerformIO $ getEnv "NIX_PATH"
      nixpkgsSourceToParam (NF.NixpkgsTarball uri) = ("nixpkgs=" <> URI.render uri)
   in case nixFlow of
        NixFlow (NixFlowConfig {NF.nixEnv, NF.nixpkgsSource, NF.command, NF.args, NF.env}) ->
          let nixPathEnvValue = nixpkgsSourceToParam nixpkgsSource
              commandConfig =
                CommandFlowConfig
                  { CF.command = "nix-shell",
                    CF.args = ("--run" : command : args) ++ packageSpec nixEnv,
                    CF.env = ("NIX_PATH", nixPathEnvValue) : env
                  }
           in reinterpret $ strand #command $ CommandFlow $ commandConfig
