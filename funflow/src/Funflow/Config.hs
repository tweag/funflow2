{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Funflow.Config where

import Control.Exception (Exception)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BSU
import Data.Maybe (mapMaybe)
import qualified Data.Scientific as SC
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON, Object, Value, decodeThrow, parseEither, (.:))
import System.Environment (lookupEnv)

type ConfigKey = Text

-- Re-using Data.Yaml types here since we are parsing config from
-- text anyways.
type ConfigMap = Object

data ExternalConfig = ExternalConfig
  { fileConfig :: ConfigMap,
    envConfig :: ConfigMap,
    cliConfig :: ConfigMap
  }
data Configurable a where

  FromFile :: FromJSON a => ConfigKey -> Configurable a
  FromEnv :: FromJSON a => ConfigKey -> Configurable a
  FromCLI :: FromJSON a => ConfigKey -> Configurable a
  Literal :: a -> Configurable a

-- Note: Errors should be raised in the interpreter, so all of this stuff just returns
-- the messages.

-- | Render a Configurable into a Literal value using a set of external configurations, returning
-- an error message if rendering failed.
render :: forall a. Configurable a -> ExternalConfig -> Either String (Configurable a)
render configVal extConfig = case configVal of
  FromFile key -> appendErrorContext key "config file" $ valueFromObject key $ fileConfig extConfig
  FromEnv key -> appendErrorContext key "environment variable" $ valueFromObject key $ fileConfig extConfig
  FromCLI key -> appendErrorContext key "CLI args" $ valueFromObject key $ fileConfig extConfig
  Literal _ -> Right configVal
  where
    valueFromObject :: FromJSON a => Text -> (Object -> Either String a)
    valueFromObject k = parseEither (.: k)

    appendErrorContext :: Text -> String -> Either String a -> Either String (Configurable a)
    appendErrorContext configKey fromConfigName parseResult = case parseResult of
      Left err -> Left $ "Failed to extract configurable " ++ unpack configKey ++ " from " ++ fromConfigName ++ " with error: " ++ err
      Right result -> Right $ Literal result

-- | Gets the config key for a configurable value, if it exists.
configId :: Configurable a -> Maybe ConfigKey
configId conf = case conf of
  FromFile k -> Just k
  FromEnv k -> Just k
  FromCLI k -> Just k
  Literal _ -> Nothing

-- | Helper for getting a required config keys from a list
-- of configurable values of the same type.
configIds :: [Configurable a] -> [ConfigKey]
configIds = mapMaybe configId

-- TODO config file, env, and cli read functions

-- readEnv :: ConfigKey -> IO Object
-- readEnv key = do
--   val <- lookupEnv $ unpack key
--   case val of
--     Nothing -> error -- Return an empty object
--     Just v -> decodeThrow BSU.fromString v
