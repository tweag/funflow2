{-# LANGUAGE QuasiQuotes #-}

module Funflow.Flows.External where

import qualified Data.CAS.ContentStore as CS
import Data.Default (Default, def)
import Data.Int (Int64)
import Funflow.Flows.Cached (MetadataWriter)

data ExternalProcessPurity = PureExternalProcess | ImpureExternalProcess (IO Int64)

instance Default ExternalProcessPurity where
  def = PureExternalProcess

alwaysRecompile :: ExternalProcessPurity
alwaysRecompile = ImpureExternalProcess randomIO

-- | Additional properties associated with external tasks.
data ExternalFlowProperties i
  = ExternalFlowProperties
      { -- Handler to write metadata to the content store.
        externalProcessMetadataWriter :: MetadataWriter i (),
        -- Specify whether this external step is pure or impure, and as such if it should be cached
        externalProcessPurity :: ExternalProcessPurity
      }

instance Default (ExternalFlowProperties i) where
  def =
    ExternalFlowProperties
      { externalProcessMetadataWriter = Nothing,
        externalProcessPurity = def
      }

data ExternalFlow i o where
  External ::
    ExternalFlowProperties i ->
    (i -> ExternalTask) ->
    ExternalFlow i CS.Item

---------------------------------------------------

-- | Set of items which may be treated as an input path to an external task.
data InputPath
  = -- | An item in the content store.
    IPItem CS.Item
  | -- | An external file whose contents are considered assured by the external
    -- system.
    IPExternalFile ExternallyAssuredFile
  | -- | An external directory whose contents are considered assured by the
    -- external system.
    IPExternalDir ExternallyAssuredDirectory
  deriving (Generic, Show)

instance ContentHashable IO InputPath

instance FromJSON InputPath

instance ToJSON InputPath

instance Store InputPath

-- | Component of a parameter
data ParamField
  = -- | Text component.
    ParamText !T.Text
  | -- | Reference to a path to a content store item.
    ParamPath !InputPath
  | -- | Reference to an environment variable.
    ParamEnv !T.Text
  | -- | Reference to the effective user ID of the executor.
    ParamUid
  | -- | Reference to the effective group ID of the executor.
    ParamGid
  | -- | Reference to the output path in the content store.
    ParamOut
  | -- | A quoted command that we can pass to another program as an
    -- argument.
    ParamCmd Param
  deriving (Generic, Show)

instance ContentHashable IO ParamField

instance FromJSON ParamField

instance ToJSON ParamField

instance Store ParamField

-- | A parameter to an external task
--
-- The runtime values to external references, e.g. environment variables,
-- should not significantly influence the result of the external task.
-- In particular, the content hash will not depend on these runtime values.
newtype Param = Param [ParamField]
  deriving (Generic, Monoid, Semigroup, Show)

instance IsString Param where
  fromString s = Param [ParamText (fromString s)]

instance ContentHashable IO Param

instance FromJSON Param

instance ToJSON Param

instance Store Param

-- | Converter of path components.
data ConvParam f
  = ConvParam
      { -- | Resolve a reference to a content store item.
        convPath :: CS.Item -> f (Path Abs Dir),
        -- | Resolve an environment variable.
        convEnv :: T.Text -> f T.Text,
        -- | Resolve the effective user ID.
        convUid :: f CUid,
        -- | Resolve the effective group ID.
        convGid :: f CGid,
        -- | Resolve the output path in the content store.
        convOut :: f (Path Abs Dir)
      }

paramFieldToText ::
  Applicative f =>
  ConvParam f ->
  ParamField ->
  f T.Text
paramFieldToText _ (ParamText txt) = pure txt
paramFieldToText c (ParamPath (IPItem item)) = T.pack . fromAbsDir <$> convPath c item
paramFieldToText _ (ParamPath (IPExternalFile (ExternallyAssuredFile item))) =
  pure . T.pack . fromAbsFile $ item
paramFieldToText _ (ParamPath (IPExternalDir (ExternallyAssuredDirectory item))) =
  pure . T.pack . fromAbsDir $ item
paramFieldToText c (ParamEnv env) = convEnv c env
paramFieldToText c ParamUid = T.pack . show <$> convUid c
paramFieldToText c ParamGid = T.pack . show <$> convGid c
paramFieldToText c ParamOut = T.pack . fromAbsDir <$> convOut c
paramFieldToText c (ParamCmd cmd) = paramToText c cmd

-- | Transform a parameter to text using the given converter.
paramToText ::
  Applicative f =>
  ConvParam f ->
  Param ->
  f T.Text
paramToText c (Param ps) = mconcat <$> traverse (paramFieldToText c) ps

stringParam :: String -> Param
stringParam str = Param [ParamText (T.pack str)]

textParam :: T.Text -> Param
textParam txt = Param [ParamText txt]

-- | Reference to a path to either:
--   - a content store item, or
--   - an externally assured file/directory.
pathParam :: InputPath -> Param
pathParam item = Param [ParamPath item]

-- | Reference to a path to a file or directory within a store item.
contentParam :: CS.Content t -> Param
contentParam (CS.All item) = pathParam $ IPItem item
contentParam (item CS.:</> path) =
  pathParam (IPItem item) <> stringParam (toFilePath path)

-- | Reference an externally assured file
externalFileParam :: ExternallyAssuredFile -> Param
externalFileParam = pathParam . IPExternalFile

-- | Reference an externally assured file
externalDirectoryParam :: ExternallyAssuredDirectory -> Param
externalDirectoryParam = pathParam . IPExternalDir

-- | Reference to an environment variable.
envParam :: T.Text -> Param
envParam env = Param [ParamEnv env]

-- | Reference to the effective user ID of the executor.
uidParam :: Param
uidParam = Param [ParamUid]

-- | Reference to the effective group ID of the executor.
gidParam :: Param
gidParam = Param [ParamGid]

-- | Reference to the output path in the content store.
outParam :: Param
outParam = Param [ParamOut]

-- | Control how and where stdout from the process is captured. Some external
-- steps will write their output to stdout rather than to a file.
data OutputCapture
  = -- | Specify that the step will write its output files directly, and that
    --   stdout will not be captured in the step output.
    NoOutputCapture
  | -- | Capture output to a file named 'out' in the output directory.
    StdOutCapture
  | -- | Capture output to a custom named file in the output directory.
    CustomOutCapture (Path Rel File)
  deriving (Generic, Show)

-- | Get the file to write output to, if this is desired.
outputCaptureToRelFile :: OutputCapture -> Maybe (Path Rel File)
outputCaptureToRelFile NoOutputCapture = Nothing
outputCaptureToRelFile StdOutCapture = Just [relfile|out|]
outputCaptureToRelFile (CustomOutCapture file) = Just file

instance ContentHashable IO OutputCapture

instance FromJSON OutputCapture

instance ToJSON OutputCapture

instance Store OutputCapture

-- | Control the environment set for the external process. This can either
--   inherit from the surrounding environment, or explicitly set things.
data Env
  = -- | Inherit all environment variables from the surrounding shell. Note that
    -- the values of these variables will not be taken into account in the
    -- content hash, and so changes to them will not trigger a rerun of the
    -- step.
    EnvInherit
  | EnvExplicit [(T.Text, Param)]
  deriving (Generic, Show)

instance ContentHashable IO Env

instance FromJSON Env

instance ToJSON Env

instance Store Env

-- | A monomorphic description of an external task. This is basically just
--   a command which can be run.
data ExternalTask
  = ExternalTask
      { _etCommand :: T.Text,
        -- | Environment variables to set for the scope of the execution.
        _etParams :: [Param],
        _etEnv :: Env,
        _etWriteToStdOut :: OutputCapture
      }
  deriving (Generic, Show)

instance ContentHashable IO ExternalTask

instance FromJSON ExternalTask

instance ToJSON ExternalTask

instance Store ExternalTask

data TaskDescription
  = TaskDescription
      { _tdOutput :: ContentHash,
        _tdTask :: ExternalTask
      }
  deriving (Generic, Show)
