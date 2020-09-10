{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Funflow.Docker
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)

main :: IO ()
main = do
  manager <- newDefaultDockerManager "linux"
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  let path = "/home/dorran/Desktop/blarg"
  let container =
        (defaultContainerSpec "ubuntu:latest")
          { cmd = ["bash", "-c", "for i in {1..1000}; do echo foo && echo thisiserror >&2; done"],
            hostVolumes = ["/home/dorran/Desktop/test-bind:/host"]
          }
  result <- runExceptT $ do
    cid <- runContainer manager container
    liftIO $ print cid
    saveContainerArchive manager uid gid "/usr/bin/bash" path cid
    saveContainerLogs manager Both "/home/dorran/Desktop/blarg/logs.txt" cid
    removeContainer manager False False cid
  case result of
    Left err -> print err
    Right _ -> print "Run succeeded! :)"
