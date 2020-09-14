{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Except (runExceptT)
import Data.Either (isLeft, isRight)
import Data.List
import Data.Ord
import qualified Data.Text as T
import Funflow.Docker (ContainerSpec (..), defaultContainerSpec, newDefaultDockerManager, removeContainer, runContainer, saveContainerArchive)
import GHC.IO.Handle (Handle)
import Network.HTTP.Client (Manager)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain (withResource manager cleanupManager dockerIntegrationTests)

-- TODO: Make integration tests work for windows as well
manager :: IO Manager
manager = newDefaultDockerManager "linux"

testImage :: T.Text
testImage = "alpine:20200626"

cleanupManager :: Manager -> IO ()
cleanupManager m = return ()

containerWithSuccessfulCommand :: ContainerSpec
containerWithSuccessfulCommand =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "exit 0"]
    }

containerWithFailingCommand :: ContainerSpec
containerWithFailingCommand =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "exit 1"]
    }

containerWithValidHostBindVolume :: FilePath -> Handle -> ContainerSpec
containerWithValidHostBindVolume p _ =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "ls /test-host-bind"],
      hostVolumes = [T.concat [T.pack p, ":/test-host-bind"]]
    }

containerWithInvalidHostBindVolume :: ContainerSpec
containerWithInvalidHostBindVolume =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "ls /test-host-bind"],
      hostVolumes = ["/does/not/exist/on/host:/test-host-bind"]
    }

containerWithKnownFile :: ContainerSpec
containerWithKnownFile =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "echo foo > /test-file.txt"]
    }

containerWithKnownDirectory :: ContainerSpec
containerWithKnownDirectory =
  (defaultContainerSpec testImage)
    { cmd = ["sh", "-c", "mkdir -p /test-dir/subdir && echo foo > /test-dir/test-file.txt"]
    }

dockerIntegrationTests :: IO Manager -> TestTree
dockerIntegrationTests managerIO =
  testGroup
    "Docker integration tests"
    [ testCase "Run container valid command" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run container invalid command" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithFailingCommand
        assertBool "Running container succeeded when it should have actually failed" $ isLeft result,
      testCase "Run container with valid host bind volume" $ do
        manager <- managerIO
        result <- withSystemTempFile "docker-client-bind" (\p h -> runExceptT $ runContainer manager $ containerWithValidHostBindVolume p h)
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand >>= removeContainer manager False False
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container forcefully" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand >>= removeContainer manager True False
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container plus its volumes" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand >>= removeContainer manager False True
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run and remove a container plus its volumes forcefully" $ do
        manager <- managerIO
        result <- runExceptT $ runContainer manager containerWithSuccessfulCommand >>= removeContainer manager True True
        assertBool ("Failed with error " ++ show result) $ isRight result,
      testCase "Run a container and extract a file from it" $ do
        manager <- managerIO
        uid <- getEffectiveUserID
        gid <- getEffectiveGroupID
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-cp-file"
            ( \p -> do
                result <- runExceptT $ runContainer manager containerWithKnownFile >>= saveContainerArchive manager uid gid "/test-file.txt" p
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    outputExists <- doesFileExist (p ++ "/test-file.txt")
                    return (outputExists, "Could not find expected file after extraction")
            )
        assertBool msg success,
      testCase "Run a container and extract a directory from it" $ do
        manager <- managerIO
        uid <- getEffectiveUserID
        gid <- getEffectiveGroupID
        (success, msg) <-
          withSystemTempDirectory
            "docker-client-cp-file"
            ( \p -> do
                result <- runExceptT $ runContainer manager containerWithKnownDirectory >>= saveContainerArchive manager uid gid "/test-dir" p
                case result of
                  Left e -> return (False, show e)
                  Right _ -> do
                    fileOutputExists <- doesFileExist (p ++ "/test-dir/test-file.txt")
                    dirOutputExists <- doesDirectoryExist (p ++ "/test-dir/subdir")
                    return (fileOutputExists && dirOutputExists, "Could not find expected file and subdirectory after extraction")
            )
        assertBool msg success
        -- TODO: Add test case for getting stdout/stderr
    ]
