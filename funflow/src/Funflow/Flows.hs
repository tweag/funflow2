{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

{-
 - Utilities to hide the kernmantle API by putting together the user effect and the strand type
 -}
module Funflow.Flows
  ( pureFlow,
    ioFlow,
    shellFlow,
    commandFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Rope (strand)
import qualified Data.CAS.ContentStore as CS
import Data.Text (Text)
import Funflow.Flow (Flow)
import Funflow.Flows.Command (CommandFlow (CommandFlow, ShellCommandFlow), CommandFlowConfig, CommandFlowInput)
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig)
import Funflow.Flows.Nix (NixFlow (NixFlow), NixFlowConfig)
import Funflow.Flows.Simple (SimpleFlow (IO, Pure))

pureFlow :: (i -> o) -> Flow i o
pureFlow f = strand #simple $ Pure f

ioFlow :: (i -> IO o) -> Flow i o
ioFlow f = strand #simple $ IO f

shellFlow :: Text -> Flow () ()
shellFlow config = strand #command $ ShellCommandFlow config

commandFlow :: CommandFlowConfig -> Flow CommandFlowInput CS.Item
commandFlow config = strand #command $ CommandFlow config

dockerFlow :: DockerFlowConfig -> CommandFlowConfig -> Flow CommandFlowInput CS.Item
dockerFlow dockerConfig commandConfig = strand #docker $ DockerFlow dockerConfig commandConfig

nixFlow :: NixFlowConfig -> CommandFlowConfig -> Flow CommandFlowInput CS.Item
nixFlow nixConfig commandConfig = strand #nix $ NixFlow nixConfig commandConfig
