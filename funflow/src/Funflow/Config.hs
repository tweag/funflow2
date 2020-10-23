{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Funflow.Config where

import Control.Exception (Exception)
import Control.Exception.Safe (throwString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Maybe (mapMaybe)
import qualified Data.Scientific as SC
import Data.Text (Text, pack, unpack)
import Data.Yaml (FromJSON, Object, ParseException, Value, decodeEither, decodeEither', decodeFileThrow, decodeThrow, encode, object, parseEither, prettyPrintParseException, (.:))
import qualified Data.Yaml as YAML
import GHC.Generics (Generic)
import Generics.Deriving.Monoid
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
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided config."
      Just v -> case (decodeEither' $ BSU.fromString v) :: Either ParseException a of
        Left parseException -> Left $ prettyPrintParseException parseException
        Right parseResult -> Right parseResult

    valueFromObject :: FromJSON a => Text -> Object -> Either String a
    valueFromObject k obj = case HashMap.lookup k obj of
      Nothing -> Left $ "Failed to find key '" ++ unpack k ++ "' in provided config."
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

data ConfigIdsBySource = ConfigIdsBySource
  { fileConfigIds :: HashSet Text,
    envConfigIds :: HashSet Text,
    cliConfigIds :: HashSet Text
  }
  deriving (Show)

instance Semigroup ConfigIdsBySource where
  (<>) m1 m2 =
    ConfigIdsBySource
      { fileConfigIds = fileConfigIds m1 <> fileConfigIds m2,
        envConfigIds = envConfigIds m1 <> envConfigIds m2,
        cliConfigIds = cliConfigIds m1 <> cliConfigIds m2
      }

instance Monoid ConfigIdsBySource where
  mempty =
    ConfigIdsBySource
      { fileConfigIds = HashSet.empty,
        envConfigIds = HashSet.empty,
        cliConfigIds = HashSet.empty
      }

configIdBySource :: Configurable a -> ConfigIdsBySource
configIdBySource conf = case conf of
  FromFile k ->
    ConfigIdsBySource
      { fileConfigIds = HashSet.fromList [k],
        envConfigIds = HashSet.empty,
        cliConfigIds = HashSet.empty
      }
  FromEnv k ->
    ConfigIdsBySource
      { fileConfigIds = HashSet.empty,
        envConfigIds = HashSet.fromList [k],
        cliConfigIds = HashSet.empty
      }
  FromCLI k ->
    ConfigIdsBySource
      { fileConfigIds = HashSet.empty,
        envConfigIds = HashSet.empty,
        cliConfigIds = HashSet.fromList [k]
      }
  _ ->
    ConfigIdsBySource
      { fileConfigIds = HashSet.empty,
        envConfigIds = HashSet.empty,
        cliConfigIds = HashSet.empty
      }

missing :: ExternalConfig -> ConfigIdsBySource -> Maybe [ConfigKey]
missing conf ids =
  let missingFileConfs = filter (not . (flip HashMap.member $ fileConfig conf)) $ HashSet.toList $ envConfigIds ids
      missingEnvConfs = filter (not . (flip HashMap.member $ envConfig conf)) $ HashSet.toList $ fileConfigIds ids
      missingCLIConfs = filter (not . (flip HashMap.member $ cliConfig conf)) $ HashSet.toList $ cliConfigIds ids
      missingConfs = missingFileConfs ++ missingEnvConfs ++ missingCLIConfs
   in case missingConfs of
        [] -> Nothing
        _ -> Just missingConfs

-- | Helper for getting a required config keys from a list
-- of configurable values of the same type.
configIds :: [Configurable a] -> [ConfigKey]
configIds = mapMaybe configId

-- | Construct an `Object` from an environment variable. This
-- will have a default type which will need to get converted to
-- the requested config type during the render method.
readEnv :: MonadIO m => ConfigKey -> m (HashMap.HashMap Text String)
readEnv key = do
  val <- liftIO $ lookupEnv $ unpack key
  case val of
    Nothing -> return HashMap.empty
    Just v -> return $ HashMap.fromList [(key, v)]

readEnvs :: MonadIO m => [ConfigKey] -> m (HashMap.HashMap Text String)
readEnvs keys = do
  envVars <- mapM readEnv keys
  return $ mconcat envVars

-- | Alias for `decodeFileThrow`
readYamlFileConfig :: (MonadIO m, FromJSON a) => FilePath -> m a
readYamlFileConfig = decodeFileThrow
