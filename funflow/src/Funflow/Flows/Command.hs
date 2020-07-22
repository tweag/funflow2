{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Command" flows allow to run commands
 -}
module Funflow.Flows.Command where

import qualified Data.CAS.ContentStore as CS
import Data.Text (Text)

-- Configure what command to run
data CommandFlowConfig = CommandFlowConfig
  { command :: Text,
    args :: [Text],
    env :: [(Text, Text)]
  }
  deriving (Show)

data CommandFlowInput = CommandFlowInput
  { workingDirectoryContent :: Maybe CS.Item
  }
  deriving (Show)

-- Command flows to run a command
data CommandFlow i o where
  CommandFlow :: CommandFlowConfig -> CommandFlow CommandFlowInput CS.Item
  DynamicCommandFlow :: CommandFlow (CommandFlowConfig, CommandFlowInput) CS.Item
  ShellCommandFlow :: Text -> CommandFlow () ()
