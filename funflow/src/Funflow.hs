{-
 - Entry point to Funflow
 -}
module Funflow
  ( -- Basics
    Flow,
    (>>>),
    toFlow,
    -- Available effects
    SimpleEffect (PureEffect, IOEffect),
    CommandEffect (CommandEffect, ShellCommandEffect),
    DockerEffect (DockerEffect),
    NixEffect (NixEffect),
    -- Run flow
    FlowExecutionConfig (..),
    CommandExecutionHandler (..),
    defaultExecutionConfig,
    runFlow,
    -- Caching
    caching,
  )
where

import Control.Arrow ((>>>))
import Control.Kernmantle.Caching (caching)
import Funflow.Effects.Command (CommandEffect (CommandEffect, ShellCommandEffect))
import Funflow.Effects.Docker (DockerEffect (DockerEffect))
import Funflow.Effects.Nix (NixEffect (NixEffect))
import Funflow.Effects.Simple (SimpleEffect (IOEffect, PureEffect))
import Funflow.Flow (Flow, toFlow)
import Funflow.Run
  ( CommandExecutionHandler (..),
    FlowExecutionConfig (..),
    defaultExecutionConfig,
    runFlow,
  )
