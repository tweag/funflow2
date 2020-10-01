{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Data.CAS.ContentStore as CS
import Funflow
  ( Flow,
    caching,
    dockerFlow,
    ioFlow,
    getDir,
    pureFlow,
    putDir,
    runFlow,
  )
import Funflow.Tasks.Docker (DockerTaskConfig (DockerTaskConfig), DockerTaskInput (DockerTaskInput), VolumeBinding (VolumeBinding))
import qualified Funflow.Tasks.Docker as DE
import Path (Abs, Dir, Path, Rel, absdir, mkRelDir, parseAbsDir, reldir, (</>))
import Path.IO (makeAbsolute)
import System.Directory (copyFile, getCurrentDirectory)
import System.Directory.Funflow (moveDirectoryContent)
import System.IO (FilePath)

-- | Helper for getting the absolute path to the tensorflow example directory
tfExampleDir :: IO (Path Abs Dir)
tfExampleDir = flip (</>) [reldir|./tensorflow-example|] =<< parseAbsDir =<< getCurrentDirectory

--------------------------------------------------------------------------------------------
------------------------------------- FLOWS ------------------------------------------------
--------------------------------------------------------------------------------------------

-- | Copies the tensorflow example to the content store so it can be used in downstream tasks
copyExampleToStore :: Flow () CS.Item
copyExampleToStore = proc _ -> do
  exampleDir <- ioFlow tfExampleDir -< ()
  exampleItem <- putDir -< exampleDir

-- | Trains a tensorflow digit recognition model and outputs the serialized model
trainModel :: Flow CS.Item CS.Item
trainModel = proc exampleItem -> do
  -- Define a volume for the example directory
  let exampleVolume = VolumeBinding {DE.item = exampleItem, DE.mount = [absdir|/tensorflow-example/|]}
  modelItem <-
    dockerFlow
      ( DockerTaskConfig
          { DE.image = "tensorflow/tensorflow:2.3.0",
            DE.command = "bash",
            -- Note: we are installing our script's requirements so we can use the default tensorflow image
            -- we could have also prepared an image with these requirements already installed.
            DE.args =
              [ "-c",
                "pip install -r /tensorflow-example/requirements.txt && \
                \ python /tensorflow-example/train-mnist.py --n_epochs 100 ."
              ]
          }
      )
      -<
        DockerTaskInput {DE.inputBindings = [exampleVolume], DE.argsVals = mempty}
  -- DockerTask outputs are written to the content store. We can get their path with getDir
  -- and print the path before returning the content store item for downstream tasks.
  modelPath <- getDir -< modelItem
  ioFlow print -< ("Training complete. The trained model was saved at: " ++ show modelPath)
  return -< modelItem

-- | Runs a trained tensorflow digit recognition model on some demo images and outputs a plot
-- of its classifications.
runModel :: Flow (CS.Item, CS.Item) CS.Item
runModel = proc (exampleItem, trainedModelItem) -> do
  -- Define volumes for the example directory and the previously trained model
  let exampleVolume = VolumeBinding {DE.item = exampleItem, DE.mount = [absdir|/tensorflow-example/|]}
  let trainedModelVolume = VolumeBinding {DE.item = trainedModelItem, DE.mount = [absdir|/trained/|]}
  modelSummaryItem <-
    dockerFlow
      ( DockerTaskConfig
          { DE.image = "tensorflow/tensorflow:2.3.0",
            DE.command = "bash",
            DE.args =
              [ "-c",
                "pip install -r /tensorflow-example/requirements.txt && \
                \ python /tensorflow-example/inference-mnist.py /trained/model /tensorflow-example/demo-images ./summary.png"
              ]
          }
      )
      -<
        DockerTaskInput {DE.inputBindings = [exampleVolume, trainedModelVolume], DE.argsVals = mempty}
  modelPath <- getDir -< modelItem
  ioFlow print -< ("Model inferencing complete. The model outputs were saved at: " ++ show modelPath)
  return -< modelItem

-- Now we can plug everything into the final Flow DAG
mainFlow :: Flow () ()
mainFlow = proc _ -> do
  tensorflowExample <- copyExampleToStore -< ()
  trainedModel <- trainModel -< tensorflowExample
  modelResult <- runModel -< (tensorflowExample, trainedModel)

main = runFlow mainFlow () :: IO ()
