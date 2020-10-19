{-# LANGUAGE GADTs #-}

module Funflow.Config where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Yaml (Object, Value)

data ConfigValue
  = IntConfig
  | BoolConfig

data Configurable where
  FromFile :: ConfigValue -> Text -> Configurable
  FromEnv :: ConfigValue -> Text -> Configurable
  FromCLI :: ConfigValue -> Text -> Configurable
  Literal :: t -> Configurable

-- Examples:
-- foo = FromFile IntConfig "foo"
-- aaa = FromEnv BoolConfig "bar.arg"
-- bbb = Literal 42

-- Re-using Data.Yaml types here since we are parsing config from
-- text anyways.
type ConfigMap = Object

class ExternalConfigEnabled a where
  getConfigurables :: a -> [Configurable]
  renderConfigurables :: a -> (ConfigMap, ConfigMap, ConfigMap) -> a

-- Define some filters for working with collections of Configurables
-- since we will need to run different effects for populating
-- each different type
envConfigurables :: [Configurable] -> [Configurable]
envConfigurables = filter isEnvConf
  where
    isEnvConf c = case c of
      FromEnv _ _ -> True
      _ -> False

fileConfigurables :: [Configurable] -> [Configurable]
fileConfigurables = filter isFileConf
  where
    isFileConf c = case c of
      FromFile _ _ -> True
      _ -> False

cliConfigurables :: [Configurable] -> [Configurable]
cliConfigurables = filter isCLIConf
  where
    isCLIConf c = case c of
      FromCLI _ _ -> True
      _ -> False

configId :: Configurable -> Maybe Text
configId conf = case conf of
  FromFile _ k -> Just k
  FromEnv _ k -> Just k
  FromCLI _ k -> Just k
  Literal _ -> Nothing

configIds :: [Configurable] -> [Text]
configIds = mapMaybe configId
