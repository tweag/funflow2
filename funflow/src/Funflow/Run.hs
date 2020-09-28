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
import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope
  ( HasKleisliIO,
    liftKleisliIO,
    perform,
    runReader,
    untwine,
    weave',
    (&),
  )
import Control.Monad (filterM)
import Control.Monad.Except (runExceptT)
import Data.CAS.ContentHashable (DirectoryContent (DirectoryContent))
import qualified Data.CAS.ContentStore as CS
import qualified Data.CAS.RemoteCache as RC
import Data.Either (isLeft)
import qualified Data.Map.Lazy as Map
import Data.String (IsString (fromString))
import qualified Data.Text as T
import Docker.API.Client (ContainerSpec (cmd), OS (OS), awaitContainer, defaultContainerSpec, hostVolumes, newDefaultDockerManager, runContainer, saveContainerArchive, workingDir)
import Funflow.Flow (Flow)
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
import Path (Abs, Dir, Path, absdir, dirname, filename, parent, parseRelDir, parseRelFile, toFilePath, (</>))
import Path.IO (copyDirRecur)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, removeDirectory, renamePath)
import System.Info (os)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)
import Data.Maybe (catMaybes)

-- * Flow execution

-- | Flow execution configuration
data RunFlowConfig = RunFlowConfig {storePath :: Path Abs Dir}

-- | Run a flow
runFlowWithConfig ::
  -- | The configuration of the flow
  RunFlowConfig ->
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlowWithConfig config flow input =
  let -- Expand config
      (RunFlowConfig {storePath}) = config
      defaultCachingId = Just 1
   in -- Run with store to enable caching (with default path to store)
      CS.withStore storePath $ \store -> do
        flow
          -- Weave tasks
          & weave' #docker (interpretDockerTask store)
          & weave' #store (interpretStoreTask store)
          & weave' #simple interpretSimpleTask
          -- Strip of empty list of strands (after all weaves)
          & untwine
          -- Define the caching
          -- The `Just n` is a number that is used to compute caching hashes, changing it will recompute all
          & runReader (localStoreWithId store $ defaultCachingId)
          -- Finally, run
          & perform input

-- | Run a flow with the default configuration
runFlow ::
  -- | The flow to run
  Flow input output ->
  -- | The input to evaluate the flow with
  input ->
  IO output
runFlow = runFlowWithConfig (RunFlowConfig {storePath = [absdir|/tmp/funflow/store/|]})

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
          handleError hash = error $ "Could not put directory " <> show dirPath <> " in store item " <> show hash
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
interpretDockerTask :: (Arrow a, HasKleisliIO m a) => CS.ContentStore -> DockerTask i o -> a i o
interpretDockerTask store (DockerTask (DockerTaskConfig {DE.image, DE.command, DE.args})) =
  liftKleisliIO $ \(DockerTaskInput {DE.inputBindings, DE.argsVals}) ->
    -- Check args placeholder fullfillment, right is value, left is unfullfilled label
    let argsFilled =
          [ ( case arg of
                Arg value -> Right value
                Placeholder label ->
                  let maybeVal = Map.lookup label argsVals
                   in case maybeVal of
                        Nothing -> Left label
                        Just val -> Right val
            )
            | arg <- args
          ]
     in -- Error if one of the required arg label is not filled
        if any isLeft argsFilled
          then
            let unfullfilledLabels = [label | (Left label) <- argsFilled]
             in -- TODO gracefully exit
                error $ "Missing arguments with labels: " ++ show unfullfilledLabels
          else do
            let argsFilledChecked = [argVal | (Right argVal) <- argsFilled]
            manager <- newDefaultDockerManager (OS os)
            uid <- getEffectiveUserID
            gid <- getEffectiveGroupID
            let defaultWorkingDirName = "workdir"
                defaultContainerWorkingDirPath = "/" ++ defaultWorkingDirName
                container =
                  (defaultContainerSpec image)
                    { workingDir = T.pack defaultContainerWorkingDirPath,
                      cmd = [command] <> argsFilledChecked,
                      hostVolumes = map fromString [(toFilePath $ CS.itemPath store item) <> ":" <> (toFilePath mount) | VolumeBinding {DE.item, DE.mount} <- inputBindings]
                    }
            runDockerResult <- runExceptT $ do
              containerId <- runContainer manager container
              awaitContainer manager containerId
              return containerId
            case runDockerResult of
              Left err -> error $ show err
              Right containerId ->
                let -- Define behaviors to pass to @CS.putInStore@
                    handleError hash = error $ "Could not put in store item " ++ show hash
                    copyDockerContainer itemPath _ =
                      do
                        copyResult <- runExceptT $ saveContainerArchive manager uid gid defaultContainerWorkingDirPath (toFilePath itemPath) containerId
                        case copyResult of
                          Left ex -> error $ show ex
                          Right _ -> do
                            -- Since docker will extract a TAR file of the container content, it creates a directory named after the requested directory's name
                            -- In order to improve the user experience, funflow moves the content of said directory to the level of the CAS item directory

                            -- Path to the folder extracted by `docker cp`
                            itemWorkdir <- (itemPath </>) <$> (parseRelDir defaultWorkingDirName)
                            -- List of directories/files inside
                            itemWorkdirDirPaths <-
                                -- Get the list of children elements of @itemWorkdir@
                                (listDirectory $ toFilePath itemWorkdir)
                                >>= -- Tries to parse the elements given by @listDirectory@ to relative directory paths 
                                    -- and keep only successful entries
                                    pure . catMaybes . map (parseRelDir @Maybe)
                                >>= -- turn into absolute paths
                                    pure . map (itemWorkdir </>)
                                >>= -- keep only directories that exists
                                    -- this also ensures that this list comprises directories only, see doc of @doesDirectoryExist@
                                    filterM (doesDirectoryExist . toFilePath)
                            itemWorkdirFilePaths <-
                                -- Get the list of children elements of @itemWorkdir@
                                (listDirectory $ toFilePath itemWorkdir)
                                >>= -- Tries to parse the elements given by @listDirectory@ to relative directory paths 
                                    -- and keep only successful entries
                                    pure . catMaybes . map (parseRelFile @Maybe)
                                >>= -- turn into absolute paths
                                    pure . map (itemWorkdir </>)
                                >>= -- keep only directories that exists
                                    -- this also ensures that this list comprises files only, see doc of @doesFileExist@
                                    filterM (doesFileExist . toFilePath)
                            -- Move directories/files
                            mapM_ (uncurry renamePath) [(toFilePath dir, toFilePath $ (parent . parent) dir </> dirname dir) | dir <- itemWorkdirDirPaths]
                            mapM_ (uncurry renamePath) [(toFilePath file, toFilePath $ (parent . parent) file </> filename file) | file <- itemWorkdirFilePaths]
                            -- After moving files and directories to item directory, remove the directory named after the working directory
                            removeDirectory $ toFilePath itemWorkdir
                            return ()
                 in CS.putInStore store RC.NoCache handleError copyDockerContainer (container, containerId, runDockerResult)
