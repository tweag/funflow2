{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines how to run your flows
module Funflow.Run
  ( runFlow,
    runFlowWithConfig,
    RunFlowConfig (..),
  )
where

import Control.Arrow (Arrow, arr)
import Control.Exception.Safe (throw, throwString)
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope
  ( HasKleisliIO,
    LooseRopeWith,
    liftKleisliIO,
    perform,
    runReader,
    untwine,
    weave',
    (&),
    type (~>),
  )
import Control.Monad.Except (runExceptT)
import Data.CAS.ContentHashable (DirectoryContent (DirectoryContent))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.Either (isLeft)
--import Funflow.Tasks.Config (ConfigTask (FileConfigTask))

import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import qualified Data.Map.Lazy as Map
import Data.Profunctor.Trans (Reader, Writer, reading, runWriter, writing)
import Data.Set (fromList)
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Data.Yaml as YAML
import Docker.API.Client
  ( ContainerSpec (cmd),
    OS (OS),
    awaitContainer,
    defaultContainerSpec,
    hostVolumes,
    newDefaultDockerManager,
    pullImage,
    runContainer,
    saveContainerArchive,
    workingDir,
  )
import Funflow.Config (ConfigMap, Configurable (Literal), ExternalConfigEnabled (..), configId, configIds)
import Funflow.Flow (RequiredCore, RequiredStrands)
import Funflow.Run.Orphans ()
import Funflow.Tasks.Docker
  ( Arg (Arg, Placeholder),
    DockerTask (DockerTask),
    DockerTaskConfig (DockerTaskConfig),
    DockerTaskInput (DockerTaskInput),
    VolumeBinding (VolumeBinding),
  )
import qualified Funflow.Tasks.Docker as DE
import Funflow.Tasks.Simple (SimpleTask (IOTask, PureTask))
import Funflow.Tasks.Store (StoreTask (GetDir, PutDir))
import GHC.Stack (HasCallStack)
import Network.HTTP.Client (Manager)
import Path (Abs, Dir, File, Path, absdir, parseRelDir, toFilePath, (</>))
import Path.IO (copyDirRecur)
import System.Directory (removeDirectory)
import System.Directory.Funflow (moveDirectoryContent)
import System.Info (os)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)

-- * Flow execution

-- | Flow execution configuration
data RunFlowConfig = RunFlowConfig
  { storePath :: Path Abs Dir,
    -- Optional config file for configuring tasks
    configFile :: Maybe (Path Abs File) -- Does this need to be an absolute path?
  }

-- | Run a flow
runFlowWithConfig ::
  -- | The configuration of the flow
  RunFlowConfig ->
  -- | The flow to run
  LooseRopeWith RequiredStrands (RequiredCore IO) input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlowWithConfig config flow input =
  let -- Expand config
      RunFlowConfig {storePath, configFile} = config
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore storePath $ \store -> do
        -- Start the manager required for docker
        manager <- newDefaultDockerManager (OS os)

        let -- Weave all strands
            weavedPipeline =
              -- TODO: Prepend a config stage here?
              flow
                -- Weave tasks
                -- TODO: Maybe add an interpretation step for config here
                & weave' #docker (interpretDockerTask manager store)
                & weave' #store (interpretStoreTask store)
                & weave' #simple interpretSimpleTask
                -- Strip of empty list of strands (after all weaves)
                & untwine

            -- TODO -> Run Task configmap writer to get the config map here

            -- TODO -> This will need to be runReader configMap (...)
            -- Collect the list of docker images that will be used from the (Cayley Writer [String]) layer
            --(dockerTaskConfigs :: [T.Text], pipelineWithImageWriter) = runWriter weavedPipeline
            ((dockerConfigs :: HashSet.HashSet T.Text, dockerImages :: [T.Text]), pipelineWithDockerConfigReader) = runWriter weavedPipeline
            wipPlaceholders = HashMap.fromList [("foo", YAML.String "bar")] :: YAML.Object
            weavePipeline' = runReader (wipPlaceholders, wipPlaceholders, wipPlaceholders) pipelineWithDockerConfigReader
            -- Run the reader layer for caching
            -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
            core = runReader (localStoreWithId store $ defaultCachingId) weavePipeline'

        print $ "HELLO! The configs are: " ++ (show dockerConfigs)
        -- Perform some IO to extract our configuration
        -- fileConfigMap <- decodeFileThrow "my-config.yaml"
        -- envConfigMap <- readFromEnv envConfigs
        -- cliConfigMap <- readFromCLI CLIConfigs
        -- merged = throwIfInvalidType mCombine [fileConfigMap, envConfigMap, cliConfigMap]
        -- selected = filterConfigToRequested merged

        -- Pull docker images if there's any
        if length dockerImages > 0
          then do
            putStrLn "Found docker images, pulling..."
            let -- Remove duplicates by converting to a list
                dockerImagesSet = fromList dockerImages
                -- How we pull the docker image
                handleDockerImage image = do
                  putStrLn $ "Pulling docker image: " ++ T.unpack image
                  pullResult <- runExceptT $ pullImage manager image
                  case pullResult of
                    Left ex ->
                      throw ex
                    Right _ ->
                      -- No error, just continue
                      mempty :: IO ()
            mapM_ handleDockerImage dockerImagesSet
          else mempty

        -- At last, run the core
        perform input core

-- | Run a flow with the default configuration
runFlow ::
  -- | The flow to run
  LooseRopeWith RequiredStrands (RequiredCore IO) input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlow = runFlowWithConfig (RunFlowConfig {storePath = [absdir|/tmp/funflow/store/|], configFile = Nothing})

-- * Interpreters

-- ** @SimpleTask@ interpreter

-- | Interpret @SimpleTask@
interpretSimpleTask :: (Arrow a, HasKleisliIO m a) => SimpleTask i o -> a i o
interpretSimpleTask simpleTask = case simpleTask of
  PureTask f -> arr f
  IOTask f -> liftKleisliIO f

-- ** @StoreTask@ interpreters

-- | Interpret @StoreTask@
interpretStoreTask :: (Arrow a, HasKleisliIO m a, RC.Cacher m RC.NoCache) => CS.ContentStore -> StoreTask i o -> a i o
interpretStoreTask store storeTask = case storeTask of
  PutDir ->
    liftKleisliIO $ \dirPath ->
      let -- Use the DirectoryContent type
          -- this will give a hash through `ContentHashable` that takes into account the content of the directory
          directoryContent = DirectoryContent dirPath
          -- Handle errors
          handleError hash = throwString $ "Could not put directory " <> show dirPath <> " in store item " <> show hash
          -- Copy recursively a directory from a DirectoryContent type
          copy :: Path Abs Dir -> DirectoryContent -> IO ()
          copy destinationPath (DirectoryContent sourcePath) = copyDirRecur sourcePath destinationPath
       in -- Use cas-store putInStore to generate the item in which to copy
          CS.putInStore store RC.NoCache handleError copy directoryContent
  GetDir ->
    -- Get path of item from store
    arr $ \item -> CS.itemPath store item

-- ** @DockerTask@ interpreter

-- | Interpret docker task
interpretDockerTask ::
  (Arrow core, HasKleisliIO m core, HasCallStack) =>
  Manager ->
  CS.ContentStore ->
  DockerTask i o ->
  (Writer (HashSet.HashSet T.Text, [T.Text]) ~> (Reader (ConfigMap, ConfigMap, ConfigMap) ~> core)) i o
interpretDockerTask manager store (DockerTask (DockerTaskConfig {DE.image, DE.command, DE.args})) =
  -- TODO: It would be cleaner if we could make configurable fields have a type class which
  -- implements something like `isConfigurable` below
  let requiredConfigs = configIds $ getConfigurables DockerTaskConfig {DE.image, DE.command, DE.args}
   in -- Add the image to the list of docker images stored in the Cayley Writer [T.Text]
      --writing (HashSet.fromList ["foo", "bar"]) $
      writing (HashSet.fromList requiredConfigs, [image]) $
        reading $ \externalConfig ->
          liftKleisliIO $ \(DockerTaskInput {DE.inputBindings, DE.argsVals}) ->
            -- Check args placeholder fullfillment, right is value, left is unfullfilled label
            let rendered = renderConfigurables (DockerTaskConfig {DE.image, DE.command, DE.args}) externalConfig
                argsFilled =
                  [ ( case arg of
                        Arg configValue -> case configValue of -- Right value
                          Literal (value :: T.Text) -> Right value
                          -- TODO
                          _ -> error "DockerTask was provided with a `Configurable` value that does not exist in the input ConfigMaps."
                        Placeholder label ->
                          let maybeVal = Map.lookup label argsVals
                           in case maybeVal of
                                Nothing -> Left label
                                Just val -> Right val
                    )
                    | arg <- DE.args rendered
                  ]
             in -- Error if one of the required arg label is not filled
                if any isLeft argsFilled
                  then
                    let unfullfilledLabels = [label | (Left label) <- argsFilled]
                     in throwString $ "Docker task failed with error: missing arguments with labels: " ++ show unfullfilledLabels
                  else do
                    let argsFilledChecked = [argVal | (Right argVal) <- argsFilled]
                    uid <- getEffectiveUserID
                    gid <- getEffectiveGroupID
                    let -- @defaultWorkingDirName@ has been chosen arbitrarly, it is both where Docker container will execute things, but also the exported folder to the content store
                        defaultWorkingDirName = "workdir"
                        defaultContainerWorkingDirPath = "/" ++ defaultWorkingDirName
                        container =
                          (defaultContainerSpec image)
                            { workingDir = T.pack defaultContainerWorkingDirPath,
                              cmd = [command] <> argsFilledChecked,
                              -- ":ro" suffix on docker binding means "read-only", the mounted volumes from the content store will not be modified
                              hostVolumes = map fromString [(toFilePath $ CS.itemPath store item) <> ":" <> (toFilePath mount) <> ":ro" | VolumeBinding {DE.item, DE.mount} <- inputBindings]
                            }
                    -- Run the docker container
                    runDockerResult <- runExceptT $ do
                      containerId <- runContainer manager container
                      awaitContainer manager containerId
                      return containerId
                    -- Process the result of the docker computation
                    case runDockerResult of
                      Left ex -> throw ex
                      Right containerId ->
                        let -- Define behaviors to pass to @CS.putInStore@
                            handleError hash = throwString $ "Could not put in store item " ++ show hash
                            copyDockerContainer itemPath _ = do
                              copyResult <- runExceptT $ saveContainerArchive manager uid gid defaultContainerWorkingDirPath (toFilePath itemPath) containerId
                              case copyResult of
                                Left ex -> throw ex
                                Right _ -> do
                                  -- Since docker will extract a TAR file of the container content, it creates a directory named after the requested directory's name
                                  -- In order to improve the user experience, funflow moves the content of said directory to the level of the CAS item directory
                                  itemWorkdir <- (itemPath </>) <$> (parseRelDir defaultWorkingDirName)
                                  moveDirectoryContent itemWorkdir itemPath
                                  -- After moving files and directories to item directory, remove the directory named after the working directory
                                  removeDirectory $ toFilePath itemWorkdir
                         in CS.putInStore store RC.NoCache handleError copyDockerContainer (container, containerId, runDockerResult)

-- interpretSimpleTask :: (Arrow a, HasKleisliIO m a) => SimpleTask i o -> a i o
-- interpretSimpleTask simpleTask = case simpleTask of
--   PureTask f -> arr f
--   IOTask f -> liftKleisliIO f

-- Configurable tasks Write their required config to a writer and read their required config from a reader
-- The singular config task performs IO to create a Map of config values. This IO happens at LOAD TIME
-- The result of this IO is what gets READ in downstream TASKS
-- configurable tasks should be wrapped like: Writer (Reader (Effect)). This way we can write the
-- required values for config validation then use the reader to read the values coming in from the config
-- The ? is whether the ConfigTask needs to return its value wrapped in a reader. I think the answer is yes

-- This is how you have an interpreter READ an input config map
-- reading $ \configMap -> returnA . Map.get configMap configKey

-- Do we really need an interpreter? i.e. are there any use cases for a core effect
-- as a part of config if config is processed at load time.

-- What if I want to access options within the docker container? Would need to mount the options file
--
--
-- READER : {MAP OF CONFIG FIELDS}
-- read config file and produce a map
-- do:
-- for each field in config fields
--  case fileconfig -> check if its in the map and return the value
--  case cliconfig -> check if we have some type of cli context and check it for the value
--  case envconfig -> read from environment and make sure it is non-empty

-- interpretConfigTask ::
--   (Arrow core, HasKleisliIO m core) =>
--   ConfigTask () o ->
--   (Reader Map.Map ~> core) () o
-- interpretConfigTask task = writing $ case task of
--   FileConfigTask getConfigPath -> do
--     -- Read the whole schebang
--     liftKleisliIO $ do
--       configPath <- getConfigPath
--       -- Read the yaml into a map
--       -- Placeholder
--       return Map.empty
-- interpretConfigTask (GetConfig configKey) = writing myMap $ reading $ \configMap -> returnA . Map.get configMap configKey