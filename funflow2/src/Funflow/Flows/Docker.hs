{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Docker" flows allow to run tasks in Docker
 -}
module Funflow.Flows.Docker where

-- Configure what task to run in Docker
data DockerFlowConfig i o
  = DockerFlowConfig
      { image :: String,
        command :: String,
        args :: [String]
      }

-- Docker flows to perform external tasks
data DockerFlow i o where
  DockerFlow :: DockerFlowConfig i o -> DockerFlow () ()
