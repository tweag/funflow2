{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Docker" flows allow to run tasks in Docker
 -}
module Funflow.Flows.Docker where

import qualified Data.CAS.ContentStore as CS
import Data.Text (Text)
import Funflow.Flows.Command (CommandFlowConfig, CommandFlowInput)

-- Configure what task to run in Docker
data DockerFlowConfig = DockerFlowConfig
  { image :: Text
  }

-- Docker flows to perform external tasks
data DockerFlow i o where
  DockerFlow :: DockerFlowConfig -> CommandFlowConfig -> DockerFlow CommandFlowInput CS.Item
