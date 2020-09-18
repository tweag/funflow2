{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
--import Data.List (sortBy)

-- import qualified Data.Map as Map
-- import Data.Ord (comparing)
-- import qualified Data.Text.IO as T
-- import Funflow
-- import qualified Data.Text as T
-- import Text.Printf (printf)
-- import Text.Regex.Posix ((=~))
import qualified Data.CAS.ContentStore as CS
import Funflow
  ( Flow,
    caching,
    dockerFlow,
    ioFlow,
    pureFlow,
    runFlow,
    putDir,
    getDir
  )
import Funflow.Effects.Docker (DockerEffectConfig (DockerEffectConfig), DockerEffectInput (DockerEffectInput), VolumeBinding (VolumeBinding))
import qualified Funflow.Effects.Docker as DE
import System.Directory (copyFile, getCurrentDirectory)
import System.IO (FilePath)
import Path.IO (makeAbsolute)
import Path (Path, Abs, Dir, Rel, absdir, mkRelDir, parseAbsDir, reldir, (</>))
-- extractFile :: CS.Item -> String -> IO String
-- extractFile item fname = do
--   cwd <- getCurrentDirectory
--   let outpath = cwd + "/" + fname :: FilePath
--   copyFile (CS.itemPath item) outpath
--   return outpath

-- -- | Helpers
-- formatPath :: String -> Path Abs Dir
-- formatPath p = makeAbsolute $ mkRelDir p

-- -- | Flows

-- formatFlow :: Flow String (Path Abs Dir) 
-- formatFlow = pureFlow formatPath

-- copyExample :: Flow String CS.Item
-- copyExample = proc examplePath -> do 
--   putDir -< (mkRelDir examplePath)

-- echoDocker :: Flow () CS.Item
-- echoDocker = proc () -> do
--   let ins = DockerEffectInput {DE.inputBindings = [], DE.argsVals = mempty}
--   dockerFlow (DockerEffectConfig {DE.image = "ubuntu", DE.command = "bash", DE.args = ["-c", "echo 'hello there' > foo.txt"]}) -< ins

-- Build the final pipeline using the task Flows defined above
--   Note: Using arrow syntax to control which inputs get passed to
--   which pipeline tasks, i.e. result_name <- task <- task_input

flow :: Flow () ()
flow = proc examplePath -> do
  -- Get the absolute path to the example directory and add it to the content addressable store (CAS)
  exampleDir <- ioFlow (\() -> return . flip (</>) [reldir|./tensorflow-example|] =<< parseAbsDir =<< getCurrentDirectory) -< ()
  exampleItem <- putDir -< exampleDir

  -- Define a volume for the example directory
  let exampleVolume = VolumeBinding {DE.item = exampleItem, DE.mount = [absdir|/tensorflow-example/|]}

  -- Run model training script and save serialized model
  modelItem <- dockerFlow (DockerEffectConfig {
    DE.image = "tensorflow/tensorflow:2.3.0", DE.command = "bash", 
    DE.args = ["-c", "ls -lah .. && pip install -r /tensorflow-example/requirements.txt && python /tensorflow-example/train-mnist.py /model && cp -r /model ."] }) 
    -< DockerEffectInput {DE.inputBindings = [exampleVolume], DE.argsVals = mempty}
  modelPath <- getDir -< modelItem
  ioFlow print -< modelPath

main = runFlow flow () :: IO ()
