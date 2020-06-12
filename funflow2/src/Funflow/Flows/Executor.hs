{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Executor" flows allow to run external tasks
 -}
module Funflow.Flows.Executor where

import Data.Text (Text)

-- Configure what external task to run
data ExecutorFlowConfig = ExecutorFlowConfig
  { command :: Text,
    args :: [Text],
    env :: [(Text, Text)]
  }

-- Executor flows to perform external tasks
data ExecutorFlow i o where
  ExecutorFlow :: ExecutorFlowConfig -> ExecutorFlow () ()
