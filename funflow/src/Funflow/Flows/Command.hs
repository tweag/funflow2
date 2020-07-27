{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Command" flows allow to run commands
 -}
module Funflow.Flows.Command where

import qualified Data.CAS.ContentStore as CS
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Path (Abs, Dir, Path)

data CommandArg
  = HashedCommandArg T.Text
  | UnhashedCommandArg T.Text
  deriving (Show)

getArgText :: CommandArg -> T.Text
getArgText (HashedCommandArg arg) = arg
getArgText (UnhashedCommandArg arg) = arg

instance IsString CommandArg where
  fromString arg = HashedCommandArg $ T.pack arg

-- Configure what command to run
data CommandFlowConfig = CommandFlowConfig
  { command :: T.Text,
    args :: [CommandArg],
    env :: [(T.Text, T.Text)],
    workingDirectory :: Maybe (Path Abs Dir)
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
  ShellCommandFlow :: T.Text -> CommandFlow () ()
