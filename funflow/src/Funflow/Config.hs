{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Funflow.Config where

import Data.Maybe (mapMaybe)
import qualified Data.Scientific as SC
import Data.Text (Text)
import Data.Yaml (FromJSON, Object, Value)

-- class ConfigValue

-- instance Num ConfigValue
-- instance Bool ConfigValue
-- data ConfigValue = MyInt | MyBool | MyScientific | MyText

-- data ConfigValue t = FromConfigOr t

--MyBool :: FromConfigOr Bool -> ConfigValue

-- MyScientific :: ConfigValue (FromConfigOr SC.Scientific)
-- MyText :: ConfigValue (FromConfigOr Text)

type ConfigKey = Text

-- Re-using Data.Yaml types here since we are parsing config from
-- text anyways.
type ConfigMap = Object

--type FromConfigOr t = Either ConfigKey t

-- -- | Any type that wants to be usable as a config needs an instance of this. Should be trivial in most common cases.
-- class FromValue a where
--   fromValue :: Value -> a

-- instance FromValue Integer where
--   fromValue v = 1

-- ? -> Should we also allow for default values here?
data Configurable a where
  FromFile :: FromJSON a => ConfigKey -> Configurable a
  FromEnv :: FromJSON a => ConfigKey -> Configurable a
  FromCLI :: FromJSON a => ConfigKey -> Configurable a
  Literal :: a -> Configurable a

-- Debugging
hello :: Configurable Integer
hello = Literal 1

goodbye :: Configurable Integer
goodbye = FromFile "foo.bar"

class ExternalConfigEnabled a where
  -- | Gets a list of all required config keys. This is the coarsest level
  -- we can collect configurables on without using an HList since the
  -- configs can be of different types.
  getConfigurableIds :: a -> [ConfigKey]

  -- | Converts all Configurables to Literal values
  renderConfigurables :: a -> (ConfigMap, ConfigMap, ConfigMap) -> a

-- Define some utility filters for working with collections of Configurables
-- since we will need to run different effects for populating
-- each different type
envConfigurables :: [Configurable a] -> [Configurable a]
envConfigurables = filter isEnvConf
  where
    isEnvConf c = case c of
      FromEnv _ -> True
      _ -> False

fileConfigurables :: [Configurable a] -> [Configurable a]
fileConfigurables = filter isFileConf
  where
    isFileConf c = case c of
      FromFile _ -> True
      _ -> False

cliConfigurables :: [Configurable a] -> [Configurable a]
cliConfigurables = filter isCLIConf
  where
    isCLIConf c = case c of
      FromCLI _ -> True
      _ -> False

configId :: Configurable a -> Maybe ConfigKey
configId conf = case conf of
  FromFile k -> Just k
  FromEnv k -> Just k
  FromCLI k -> Just k
  Literal _ -> Nothing

configIds :: [Configurable a] -> [ConfigKey]
configIds = mapMaybe configId
