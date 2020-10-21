{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

-- | Run commands using Docker
module Funflow.Tasks.Docker
  ( DockerTaskConfig (..),
    DockerTask (..),
    DockerTaskInput (..),
    VolumeBinding (..),
    Arg (..),
  )
where

import Data.CAS.ContentStore as CS
import Data.Map (Map)
import Data.HashMap.Strict ((!))
import Data.String (IsString, fromString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml (Object, Parser, parseMaybe, (.:))
import Funflow.Config (ConfigMap, Configurable (..), ExternalConfigEnabled (..), configIds, render)
import Path (Abs, Dir, Path)

-- | Configure what task to run in Docker
data DockerTaskConfig = DockerTaskConfig
  { -- | The name of the docker image
    image :: Text,
    -- | The command to run
    command :: Text,
    -- | The arguments to pass to the command run inside of the container
    args :: [Arg]
  }

foo = render $ Literal $ T.pack "foo"

-- instance ExternalConfigEnabled DockerTaskConfig where
--   getConfigurableIds dockerTaskConfig = configIds $ foldl get [] (args dockerTaskConfig)
--     where
--       get acc arg = case arg of
--         Arg conf -> acc ++ [conf]
--         _ -> acc

--   mapConfig dockerTaskConfig f =
--     let mapArg arg = case arg of
--           Arg configurable -> f configurable
--           _ -> arg
--      in dockerTaskConfig {args = map mapArg $ args dockerTaskConfig}

--   withConfig dockerTaskConfig f = []

-- renderConfigurables dockerTaskConfig (file, env, cli) =
--   let renderedArgs = foldl (render (file, env, cli)) [] $ args dockerTaskConfig
--    in dockerTaskConfig {args = renderedArgs}
--   where
--     render :: (ConfigMap, ConfigMap, ConfigMap) -> [Arg] -> Arg -> [Arg]
--     render (fileConfig, envConfig, cliConfig) acc arg = case arg of
--       Arg c -> case c of
--         FromFile key -> acc ++ [checkIfMaybe arg $ valueFromObject key fileConfig]
--         FromEnv key -> acc ++ [checkIfMaybe arg $ valueFromObject key envConfig]
--         FromCLI key -> acc ++ [checkIfMaybe arg $ valueFromObject key cliConfig]
--         Literal _ -> acc ++ [arg]
--       _ -> acc ++ [arg]
--     valueFromObject :: Text -> (Object -> Maybe Text)
--     valueFromObject k = parseMaybe (.: k)
--     checkIfMaybe :: Arg -> Maybe Text -> Arg
--     checkIfMaybe arg val = case val of
--       Just v -> Arg $ Literal v
--       Nothing -> arg

-- TODO: Missing a constraint indicating that Arg has to be a Text

-- | Represent an argument to pass to the command run inside of a Docker container
data Arg
  = -- | Raw text argument
    Arg (Configurable Text)
  | -- | A placeholder for an argument to be passed as runtime input to the task (filled by @argsVals@)
    Placeholder String

-- | Input to a Docker task to finalize its configuration
data DockerTaskInput = DockerTaskInput
  { -- | Input items to mount on the container
    inputBindings :: [VolumeBinding],
    -- | A map representing how to fill the argument placeholders (placeholder label -> argument value)
    argsVals :: Map String Text
  }

-- | Represent how to bind a directory from cas-store (@CS.Item@) to a container internal file system
data VolumeBinding = VolumeBinding {item :: CS.Item, mount :: Path Abs Dir}

-- Docker tasks to perform external tasks
data DockerTask i o where
  DockerTask :: DockerTaskConfig -> DockerTask DockerTaskInput CS.Item
