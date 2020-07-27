{-# LANGUAGE Arrows #-}
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
import Data.CAS.ContentHashable (ContentHash, contentHash)
import qualified Data.CAS.ContentHashable as CH
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.String (fromString)
import qualified Data.Text as T
import Funflow.Flow (Flow)
import Funflow.Flows.Command
  ( CommandArg,
    CommandFlow (CommandFlow, DynamicCommandFlow, ShellCommandFlow),
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
import Path.IO (copyDirRecur)
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
          & weave #docker (interpretDockerFlow store)
          & weave #nix (interpretNixFlow store)
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
  PureFlow f -> arr f
  IOFlow f -> liftKleisliIO f

--
-- Possible interpreters for the command flow
--

-- System executor: just spawn processes

commandFlowSystemExecutorContentHash :: T.Text -> [CommandArg] -> Maybe CS.Item -> IO ContentHash
commandFlowSystemExecutorContentHash command args workingDirectoryContent =
  let hashedArgs = [arg | CF.HashedCommandArg arg <- args]
   in CH.contentHash (command, hashedArgs, workingDirectoryContent)

interpretCommandFlowSystemExecutor :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> CommandFlow i o -> a i o
interpretCommandFlowSystemExecutor store commandFlow =
  let runCommandFlow :: CommandFlowConfig -> CommandFlowInput -> IO CS.Item
      runCommandFlow (CommandFlowConfig {CF.command, CF.args}) (CommandFlowInput {CF.workingDirectoryContent}) =
        do
          -- Make a content store item
          itemHash <- commandFlowSystemExecutorContentHash command args workingDirectoryContent
          CS.Complete (_, completedItem) <- CS.withConstructIfMissing store RC.NoCache mempty itemHash $ \outputContentItemFilePath ->
            do
              -- Optionally, copy workingDirectoryContent to output item file path
              case workingDirectoryContent of
                Just workingDirectoryContentItem -> do
                  let inputDirectoryFilePath = CS.itemPath store workingDirectoryContentItem
                  copyDirRecur inputDirectoryFilePath outputContentItemFilePath
              -- Run task with cwd set to output item file path
              _ <-
                createProcess $
                  CreateProcess
                    { cmdspec = RawCommand (T.unpack command) (fmap (T.unpack . CF.getArgText) args),
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

-- External executor: use the external-executor package
-- TODO currently little to no benefits, need to allow setting SQL, Redis, etc
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
          liftKleisliIO $ \(CommandFlowInput {CF.workingDirectoryContent}) ->
            -- Create the task description (task + cache hash)
            -- TODO do something of `workingDirectoryContent`
            let task :: ExternalTask
                task =
                  ExternalTask
                    { _etCommand = command,
                      -- TODO use input env
                      _etEnv = EnvExplicit [(x, (fromString . T.unpack) y) | (x, y) <- env],
                      -- TODO use input args
                      _etParams = fmap (fromString . T.unpack . CF.getArgText) args,
                      _etWriteToStdOut = StdOutCapture
                    }
             in do runTask task
        DynamicCommandFlow ->
          liftKleisliIO $ \((CommandFlowConfig {CF.command, CF.args, CF.env}), CommandFlowInput {CF.workingDirectoryContent}) ->
            -- Create the task description (task + cache hash)
            -- TODO do something of `workingDirectoryContent`
            let task :: ExternalTask
                task =
                  ExternalTask
                    { _etCommand = command,
                      -- TODO use input env
                      _etEnv = EnvExplicit [(x, (fromString . T.unpack) y) | (x, y) <- env],
                      -- TODO use input args
                      _etParams = fmap (fromString . T.unpack . CF.getArgText) args,
                      _etWriteToStdOut = StdOutCapture
                    }
             in do runTask task
        ShellCommandFlow shellCommand ->
          liftKleisliIO $ \_ ->
            let task =
                  ExternalTask
                    { _etCommand = shellCommand,
                      -- TODO use input env
                      _etEnv = EnvExplicit [],
                      -- TODO use input args
                      _etParams = [],
                      _etWriteToStdOut = StdOutCapture
                    }
             in do
                  _ <- runTask task
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
interpretDockerFlow :: CS.ContentStore -> WeaverFor "docker" DockerFlow '[ '("simple", SimpleFlow), '("command", CommandFlow)] '[Arrow]
interpretDockerFlow store reinterpret dockerFlow = case dockerFlow of
  DockerFlow (DockerFlowConfig {DF.image}) (CommandFlowConfig {CF.command, CF.args}) ->
    let -- Define a IO function that will later be wrapped in an arrow
        mkCommandConfig :: CommandFlowInput -> IO CommandFlowConfig
        mkCommandConfig (CommandFlowInput {CF.workingDirectoryContent}) = do
          -- Specify the list of args to the `docker` command for the hash computation
          let dockerHashedArgs :: [CommandArg]
              dockerHashedArgs =
                CF.HashedCommandArg "run"
                  : CF.HashedCommandArg image
                  : CF.HashedCommandArg command
                  : args
          hash <- commandFlowSystemExecutorContentHash command dockerHashedArgs workingDirectoryContent
          CS.Complete item <- CS.lookup store hash
          let outputPath = CS.itemPath store item
          -- args passed to run the docker command
          -- WARNING all hashed args must be specified in the above list `dockerHashedArgs`, in the same order
          let dockerArgs =
                CF.HashedCommandArg "run"
                  : CF.HashedCommandArg image
                  : CF.UnhashedCommandArg "-v"
                  : CF.UnhashedCommandArg ((T.pack $ fromAbsDir outputPath) <> ":/out")
                  : CF.HashedCommandArg command
                  : args
          return $
            CommandFlowConfig
              { CF.command = "docker",
                CF.args = dockerArgs,
                CF.env = [],
                CF.workingDirectory = Nothing
              }
     in reinterpret $ proc commandInput -> do
          commandConfig <- strand #simple $ IOFlow $ mkCommandConfig -< commandInput
          strand #command $ DynamicCommandFlow -< (commandConfig, commandInput)

-- Interpret nix flow
interpretNixFlow :: CS.ContentStore -> WeaverFor "nix" NixFlow '[ '("simple", SimpleFlow), '("command", CommandFlow)] '[Arrow]
interpretNixFlow store reinterpret nixFlow =
  case nixFlow of
    NixFlow (NixFlowConfig {NF.nixEnv, NF.nixpkgsSource}) (CommandFlowConfig {CF.command, CF.args, CF.env}) ->
      let -- Define a IO function that will later be wrapped in an arrow
          mkCommandConfig :: CommandFlowInput -> IO CommandFlowConfig
          mkCommandConfig (CommandFlowInput {CF.workingDirectoryContent}) = do
            -- Retrieve nix path
            nixPathEnvValue <- NF.nixpkgsSourceToParam nixpkgsSource
            -- Specify the list of args to the `nix-shell` command for the hash computation
            let nixHashedArgs :: [CommandArg]
                nixHashedArgs =
                  ( CF.HashedCommandArg "--run"
                      : CF.HashedCommandArg command
                      : args
                  )
                    ++ (map CF.HashedCommandArg $ NF.packageSpec nixEnv)
            hash <- commandFlowSystemExecutorContentHash command nixHashedArgs workingDirectoryContent
            CS.Complete item <- CS.lookup store hash
            let outputPath = CS.itemPath store item
            -- args passed to run the nix command
            -- WARNING all hashed args must be specified in the above list `dockerHashedArgs`, in the same order
            let nixArgs = nixHashedArgs
            return $
              CommandFlowConfig
                { CF.command = "docker",
                  CF.args = nixArgs,
                  CF.env = ("NIX_PATH", nixPathEnvValue) : env,
                  CF.workingDirectory = Just outputPath
                }
       in reinterpret $ proc commandInput -> do
            commandConfig <- strand #simple $ IOFlow $ mkCommandConfig -< commandInput
            strand #command $ DynamicCommandFlow -< (commandConfig, commandInput)
