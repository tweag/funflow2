{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Simple" flows allow to use pure functions or IO monadic continuations
 -}
module Funflow.Flows.Simple where

import Data.Default (Default (def))

-- The set of properties for running external tasks
data SimpleFlowProperties i o = ExternalFlowProperties {}

-- Default properties for running external tasks
instance Default (SimpleFlowProperties i o) where
  def :: SimpleFlowProperties i o
  def = ExternalFlowProperties {}

-- External flows to perform external tasks
data SimpleFlow i o where
  Pure :: SimpleFlowProperties i o -> (i -> o) -> SimpleFlow i o
  IO :: SimpleFlowProperties i o -> (i -> IO o) -> SimpleFlow i o
