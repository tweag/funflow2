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
    commandFlow,
    dynamicCommandFlow,
    shellFlow,
    dockerFlow,
    nixFlow,
  )
where

import Control.Kernmantle.Rope (strand)
import qualified Data.CAS.ContentStore as CS
import Data.Text (Text)
import Funflow.Flow (Flow)
import Funflow.Flows.Command (CommandFlow (CommandFlow, DynamicCommandFlow, ShellCommandFlow), CommandFlowConfig, CommandFlowInput)
import Funflow.Flows.Docker (DockerFlow (DockerFlow), DockerFlowConfig)
import Funflow.Flows.Nix (NixFlow (NixFlow), NixFlowConfig)
import Funflow.Flows.Simple (SimpleFlow (IOFlow, PureFlow))

pureFlow :: (i -> o) -> Flow i o
pureFlow f = strand #simple $ PureFlow f

ioFlow :: (i -> IO o) -> Flow i o
ioFlow f = strand #simple $ IOFlow f

shellFlow :: Text -> Flow () ()
shellFlow config = strand #command $ ShellCommandFlow config

commandFlow :: CommandFlowConfig -> Flow CommandFlowInput CS.Item
commandFlow config = strand #command $ CommandFlow config

dynamicCommandFlow :: Flow (CommandFlowConfig, CommandFlowInput) CS.Item
dynamicCommandFlow = strand #command $ DynamicCommandFlow

dockerFlow :: DockerFlowConfig -> CommandFlowConfig -> Flow CommandFlowInput CS.Item
dockerFlow dockerConfig commandConfig = strand #docker $ DockerFlow dockerConfig commandConfig

nixFlow :: NixFlowConfig -> CommandFlowConfig -> Flow CommandFlowInput CS.Item
nixFlow nixConfig commandConfig = strand #nix $ NixFlow nixConfig commandConfig
