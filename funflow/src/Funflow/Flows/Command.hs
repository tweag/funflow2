{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
 - "Command" flows allow to run commands
 -}
module Funflow.Flows.Command where

import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentHashable (ContentHash, ContentHashable, contentHash)
import qualified Data.CAS.ContentStore as CS
import Data.String (IsString, fromString)
import qualified Data.Text as T
import Path (Abs, Dir, Path)
import GHC.Generics (Generic)

-- * @CommandFlow@ definition

-- | A command argument can be used to compute the CAS Store hash or not
--
--   e.g. Docker volume binding '-v' is not relevant to compute the hash
data CommandArg
  = HashedCommandArg T.Text
  | UnhashedCommandArg T.Text
  deriving (Show, Generic)

-- | For practicity, CommandArg can be built from @String@ directly using the 'OverloadedStrings' language extension
instance IsString CommandArg where
  fromString arg = HashedCommandArg $ T.pack arg

-- | Get the 'T.Text' from any value of type @CommandArg@
getArgText :: CommandArg -> T.Text
getArgText (HashedCommandArg arg) = arg
getArgText (UnhashedCommandArg arg) = arg

-- | Configure what command to run
data CommandFlowConfig = CommandFlowConfig
  { command :: T.Text,
    args :: [CommandArg],
    env :: [(T.Text, T.Text)],
    workingDirectory :: Maybe (Path Abs Dir)
  }
  deriving (Show, Generic)

-- | The input of a command flow
--
--   Empty for now
data CommandFlowInput = CommandFlowInput
  {
  }
  deriving (Show, Generic)

-- | Command flows to run a command
data CommandFlow i o where
  -- | Run a command statically: command and args are known at load time
  CommandFlow :: CommandFlowConfig -> CommandFlow CommandFlowInput CS.Item
  -- | Run a command dynamically. If command and args are known at load time, use @CommandFlow@
  DynamicCommandFlow :: CommandFlow (CommandFlowConfig, CommandFlowInput) CS.Item
  -- | Run a shell command
  ShellCommandFlow :: T.Text -> CommandFlow () ()

-- * Hash computation

instance (MonadIO m) => ContentHashable m CommandArg

instance (MonadIO m) => ContentHashable m CommandFlowConfig where
  contentHash :: (MonadIO m) => CommandFlowConfig -> m ContentHash
  contentHash (CommandFlowConfig {command, args, env, workingDirectory}) =
    let hashedArgs = [arg | HashedCommandArg arg <- args]
     in contentHash (command, hashedArgs, env, workingDirectory)

instance (MonadIO m) => ContentHashable m CommandFlowInput
