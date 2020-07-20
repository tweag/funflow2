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

data WritingCommandFlowInput = WritingCommandFlowInput
  { workingDirectoryContent :: CS.Item
  }

-- Command flows to run a command
data CommandFlow i o where
  -- Working directory will be the current working directory
  CommandFlow :: CommandFlowConfig -> CommandFlow () ()
  ShellCommandFlow :: Text -> CommandFlow () ()
  -- Working directory will be defined and returned
  WritingCommandFlow :: CommandFlowConfig -> CommandFlow WritingCommandFlowInput CS.Item
