{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "External" flows allow to run external tasks
 -}
module Funflow.Flows.External where

-- Configure what external task to run
data ExternalFlowConfig i o
  = ExternalFlowConfig
      { command :: String,
        args :: [String],
        env :: [String]
      }

-- External flows to perform external tasks
data ExternalFlow i o where
  ExternalFlow :: ExternalFlowConfig i o -> ExternalFlow () ()
