{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run your flows
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
  ( (&),
    Entwines,
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
  )
import Control.Monad.IO.Class (liftIO)
import Data.CAS.ContentHashable (contentHash)
import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.String (fromString)
import qualified Data.Text as T
import Funflow.Flow (Flow)
import Funflow.Flows.Command
  ( CommandFlow (CommandFlow, DynamicCommandFlow, ShellCommandFlow),
    CommandFlowConfig (CommandFlowConfig),
    CommandFlowInput (CommandFlowInput),
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
import Funflow.Util (mapPair)
import Funflow.Flows.Simple (SimpleFlow (IOFlow, PureFlow))
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
import System.IO (stdout)
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

-- * Run a flow

-- ** Flow execution configuration

-- | Configure how to run your flow
data FlowExecutionConfig = FlowExecutionConfig
  { -- | The command execution configuration
    commandExecution :: CommandExecutionHandler
  }

-- | Configure how to execute the commands
data CommandExecutionHandler
  = -- | Execute on the system
    SystemExecutor
  | -- | Use "external-executor" execution system
    ExternalExecutor

-- | Default configuration to run a flow
defaultExecutionConfig :: FlowExecutionConfig
defaultExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}

-- ** Flow execution

-- | Run a flow
runFlow ::
  -- | The execution configuration
  FlowExecutionConfig ->
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow against
  input ->
  IO output
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

-- * Interpreters

-- ** @SimpleFlow@ interpreter

-- | Interpret @SimpleFlow@
interpretSimpleFlow :: (Arrow a, HasKleisliIO m a) => SimpleFlow i o -> a i o
interpretSimpleFlow simpleFlow = case simpleFlow of
  PureFlow f -> arr f
  IOFlow f -> liftKleisliIO f

-- ** @CommandFlow@ interpreters

-- | Spawn a process using the 'process' library
interpretCommandFlowSystemExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowSystemExecutor store commandFlow =
  let runCommandFlow :: CommandFlowConfig -> CommandFlowInput -> IO CS.Item
      runCommandFlow commandFlowConfig _ =
        do
          -- Make a content store item
          itemHash <- CH.contentHash (commandFlowConfig)
          let (CommandFlowConfig {CF.command, CF.args, CF.env}) = commandFlowConfig
          CS.Complete (_, completedItem) <- CS.withConstructIfMissing store RC.NoCache mempty itemHash $ \outputContentItemFilePath ->
            do
              -- Run task with cwd set to output item file path
              -- TODO handle process result
              _ <-
                createProcess $
                  CreateProcess
                    { cmdspec = RawCommand (T.unpack command) (fmap (T.unpack . CF.getArgText) args),
                      env = Just $ map (mapPair T.unpack) env,
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
              return $ Right ()
          -- Return written CS Item
          return completedItem
   in case commandFlow of
        -- In this interpreter, we copy the content of the input CS.Item to a new CS.Item and
        -- use it as the current working directory for the command
        CommandFlow commandFlowConfig ->
          liftKleisliIO $ \commandFlowInput -> runCommandFlow commandFlowConfig commandFlowInput
        DynamicCommandFlow ->
          liftKleisliIO $ \(commandFlowConfig, commandFlowInput) -> runCommandFlow commandFlowConfig commandFlowInput
        ShellCommandFlow shellCommand ->
          liftKleisliIO $ \() ->
            do
              _ <-
                createProcess $
                  CreateProcess
                    { cmdspec = ShellCommand (T.unpack shellCommand),
                      env = Nothing,
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

-- | Spawn a process using the 'external-executor' package
--
--   TODO currently little to no benefits, need to allow setting SQL, Redis, etc
interpretCommandFlowExternalExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowExternalExecutor store commandFlow =
  let runTask :: ExternalTask -> IO CS.Item
      runTask task = do
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
        CS.Complete completedItem <- CS.lookup store hash
        return completedItem
   in case commandFlow of
        CommandFlow (CommandFlowConfig {CF.command, CF.args, CF.env}) ->
          liftKleisliIO $ \(CommandFlowInput {}) ->
            let task :: ExternalTask
                task =
                  ExternalTask
                    { _etCommand = command,
                      _etEnv = EnvExplicit [(x, (fromString . T.unpack) y) | (x, y) <- env],
                      _etParams = fmap (fromString . T.unpack . CF.getArgText) args,
                      _etWriteToStdOut = StdOutCapture
                    }
             in do runTask task
        DynamicCommandFlow ->
          liftKleisliIO $ \((CommandFlowConfig {CF.command, CF.args, CF.env}), CommandFlowInput {}) ->
            let task :: ExternalTask
                task =
                  ExternalTask
                    { _etCommand = command,
                      _etEnv = EnvExplicit [(x, (fromString . T.unpack) y) | (x, y) <- env],
                      _etParams = fmap (fromString . T.unpack . CF.getArgText) args,
                      _etWriteToStdOut = StdOutCapture
                    }
             in do runTask task
        ShellCommandFlow _ -> error "Shell command is not compatible with 'external-executor'"

-- ** @DockerFlow@ interpreter

-- | Interpret docker flow
interpretDockerFlow :: WeaverFor "docker" DockerFlow '[ '("simple", SimpleFlow), '("command", CommandFlow)] '[Arrow]
interpretDockerFlow reinterpret dockerFlow = case dockerFlow of
  DockerFlow (DockerFlowConfig {DF.image}) (CommandFlowConfig {CF.command, CF.args, CF.env, CF.workingDirectory}) ->
    let -- Define a IO function that will later be wrapped in an arrow
        mkCommandConfig :: CommandFlowInput -> IO CommandFlowConfig
        mkCommandConfig (CommandFlowInput {}) = do
          let dockerArgs =
                CF.HashedCommandArg "run"
                  : CF.HashedCommandArg image
                  -- TODO bind output
                  -- : CF.UnhashedCommandArg "-v"
                  -- : CF.UnhashedCommandArg ((T.pack $ fromAbsDir workingDirectory) <> ":/out")
                  : CF.HashedCommandArg command
                  : args
          return $
            CommandFlowConfig
              { CF.command = "docker",
                CF.args = dockerArgs,
                CF.env = env,
                CF.workingDirectory = workingDirectory
              }
     in reinterpret $ proc commandInput -> do
          commandConfig <- strand #simple $ IOFlow $ mkCommandConfig -< commandInput
          strand #command $ DynamicCommandFlow -< (commandConfig, commandInput)

-- ** @NixFlow@ interpreter

-- Interpret Nix flow
interpretNixFlow :: WeaverFor "nix" NixFlow '[ '("simple", SimpleFlow), '("command", CommandFlow)] '[Arrow]
interpretNixFlow reinterpret nixFlow =
  case nixFlow of
    NixFlow (NixFlowConfig {NF.nixEnv, NF.nixpkgsSource}) (CommandFlowConfig {CF.command, CF.args, CF.env, CF.workingDirectory}) ->
      let -- Define a IO function that will later be wrapped in an arrow
          mkCommandConfig :: CommandFlowInput -> IO CommandFlowConfig
          mkCommandConfig (CommandFlowInput {}) = do
            -- Retrieve nix path
            nixPathEnvValue <- NF.nixpkgsSourceToParam nixpkgsSource
            -- Specify the list of args to the `nix-shell` command
            let nixArgs =
                  ( CF.HashedCommandArg "--run"
                      : CF.HashedCommandArg command
                      : args
                  )
                    ++ (map CF.HashedCommandArg $ NF.packageSpec nixEnv)
            return $
              CommandFlowConfig
                { CF.command = "docker",
                  CF.args = nixArgs,
                  CF.env = ("NIX_PATH", nixPathEnvValue) : env,
                  CF.workingDirectory = workingDirectory
                }
       in reinterpret $ proc commandInput -> do
            commandConfig <- strand #simple $ IOFlow $ mkCommandConfig -< commandInput
            strand #command $ DynamicCommandFlow -< (commandConfig, commandInput)

-- * Utils

-- | A type alias to clarify the type of functions that will reinterpret
--   to use for interpretation functions that will be called by `weave`
type WeaverFor name eff strands coreConstraints =
  forall mantle core i o.
  (Entwines (LooseRope mantle core) strands, SatisfiesAll core coreConstraints) =>
  (forall x y. (LooseRope ('(name, eff) ': mantle) core x y -> core x y)) ->
  eff i o ->
  core i o
