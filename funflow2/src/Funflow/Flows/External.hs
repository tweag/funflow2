{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "External" flows allow to run external tasks
 -}
module Funflow.Flows.External where

import Data.Default (Default (def))

-- The set of properties for running external tasks
data ExternalFlowProperties i o
  = ExternalFlowProperties
      { name :: String
      }

-- Default properties for running external tasks
instance Default (ExternalFlowProperties i o) where
  def :: ExternalFlowProperties i o
  def =
    ExternalFlowProperties
      { name = "unnamed"
      }

-- External flows to perform external tasks
data ExternalFlow i o where
  RunCommand :: ExternalFlowProperties i o -> cmd -> ExternalFlow i ()
