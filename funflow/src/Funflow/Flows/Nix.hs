{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-
 - "Nix" flows allow to run tasks in Docker
 -}
module Funflow.Flows.Nix where

import Data.CAS.ContentHashable
  ( ContentHashable,
    contentHashUpdate,
    contentHashUpdate_fingerprint,
  )
import qualified Data.CAS.ContentStore as CS
import Data.Text (Text)
import Funflow.Flows.Command (CommandFlowConfig, CommandFlowInput)
import GHC.Generics (Generic)
import Text.URI (URI)
import qualified Text.URI as URI

data NixpkgsSource
  = -- | Inherit the @NIX_PATH@ from the environment
    NIX_PATH
  | -- | The 'URI' pointing to a nixpkgs tarball
    NixpkgsTarball URI
  deriving (Generic)

-- Configure what task to run in Docker
data NixFlowConfig = NixFlowConfig
  { -- | Specification of the nix environment
    nixEnv :: Environment,
    -- | Which version of nixpkgs to use
    nixpkgsSource :: NixpkgsSource
  }

data Environment
  = -- | Path to a shell.nix file
    ShellFile (Text)
  | -- | A list of packages that
    -- will be passed by @-p@.
    PackageList [Text]
  deriving (Generic)

-- instances for cashing
instance ContentHashable IO Environment

instance ContentHashable IO NixpkgsSource where
  contentHashUpdate ctx NIX_PATH = contentHashUpdate_fingerprint ctx NIX_PATH
  contentHashUpdate ctx (NixpkgsTarball s) = contentHashUpdate ctx (URI.render s)

-- Docker flows to perform external tasks
data NixFlow i o where
  NixFlow :: NixFlowConfig -> CommandFlowConfig -> NixFlow CommandFlowInput CS.Item
