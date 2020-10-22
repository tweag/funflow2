{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Funflow.Config where

import Control.Exception (Exception)
import Control.Exception.Safe (throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (mapMaybe)
import qualified Data.Scientific as SC
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON, Object, ParseException, Value, decodeEither, decodeEither', decodeFileThrow, decodeThrow, encode, object, parseEither, prettyPrintParseException, (.:))
import qualified Data.Yaml as YAML
import System.Environment (lookupEnv)

type ConfigKey = Text

-- Re-using Data.Yaml types here since we are parsing config from
-- text anyways.
type ConfigMap = Object

type EnvConfigMap = HashMap.HashMap Text String
data ExternalConfig = ExternalConfig

  { fileConfig :: ConfigMap,
    envConfig :: EnvConfigMap,
    cliConfig :: ConfigMap
  }
  deriving (Show)

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
  FromEnv key -> appendErrorContext key "environment variable" $ valueFromStrings key $ envConfig extConfig
  FromCLI key -> appendErrorContext key "CLI args" $ valueFromObject key $ cliConfig extConfig
  Literal _ -> Right configVal
  where
    valueFromStrings :: FromJSON a => Text -> EnvConfigMap -> Either String a
    valueFromStrings k env = case HashMap.lookup k env of
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided environment variables"
      Just v -> case (decodeEither' $ BSU.fromString v) :: Either ParseException a of
        Left parseException -> Left $ prettyPrintParseException parseException
        Right parseResult -> Right parseResult

    valueFromObject :: FromJSON a => Text -> Object -> Either String a
    valueFromObject k obj = case HashMap.lookup k obj of
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided file config"
      Just v -> case (decodeEither' $ encode v) :: Either ParseException a of
        Left parseException -> Left $ prettyPrintParseException parseException
        Right parseResult -> Right parseResult

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

-- | Construct an `Object` from an environment variable. This
-- will have a default type which will need to get converted to
-- the requested config type during the render method.
readEnv :: MonadIO m => ConfigKey -> m (HashMap.HashMap Text String)
readEnv key = do
  val <- liftIO $ lookupEnv $ unpack key
  case val of
    Nothing -> return HashMap.empty
    Just v -> return $ HashMap.fromList [(key, v)]

-- | Alias for `decodeFileThrow`
readYamlFileConfig :: (MonadIO m, FromJSON a) => FilePath -> m a
readYamlFileConfig = decodeFileThrow
