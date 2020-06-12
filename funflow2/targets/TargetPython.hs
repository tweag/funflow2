{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}

module Funflow.Targets.Example1 where

import Funflow
import Path

srcDir = [absdir|/dataset/scripts|]

flow :: () -> [Prediction]
flow = proc () -> do
  -- Download dataset (X)
  dataset <- \() ->
    python
      { src = srcDir,
        entrypoint = [relfile|download_dataset.py|],
        args = ["--url", "http://company.com/api/dataset.zip"],
        executor = Docker {image = "python"}
      }
      -<
        ()
  -- Download model
  model <- \() ->
    python
      { src = srcDir,
        entrypoint = [relfile|preload_model.py|],
        args = ["--hub-url", "http://github.com/company/models", "--model-name", "awesomenet"],
        executor = Docker {image = "pytorch/pytorch"}
      }
      -<
        ()
  -- Make predictions
  predictions <- \(dataset, model) ->
    python
      { src = srcDir,
        entrypoint = [relfile|predict.py|],
        args = ["--datasetset", dataset </> [relfile|datasetset.csv|], "--model", model </> [relfile|model.pt|]],
        executor = Docker {image = "pytorch/pytorch", inputs = [dataset, model]}
      }
      -<
        (dataset, model)
  -- Read predictions
  readFileFromStoreItem [relfile|predictions.txt|] -< predictions

main = print $ runFlow flow ()
