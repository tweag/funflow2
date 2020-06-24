{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "External" flows allow to run external tasks
 -}
module Funflow.Flows.External where

import Data.Text (Text)

-- Configure what external task to run
data ExternalFlowConfig
  = ExternalFlowConfig
      { command :: Text,
        args :: [Text],
        env :: [(Text, Text)]
      }

-- External flows to perform external tasks
data ExternalFlow i o where
  ExternalFlow :: ExternalFlowConfig -> ExternalFlow () ()
