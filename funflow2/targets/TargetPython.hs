{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}

module Funflow.Targets.Example1 where

import Funflow
import Path

srcDir = [absdir|/data/scripts|]

flow :: () -> [Prediction]
flow = proc () -> do
  -- Download data (X)
  data <- \() -> python {
      src = srcDir,
      entrypoint = [relfile|download_data.py|],
      args = ["--url", "http://company.com/api/data.zip"],
      executor = Docker { image = "python" }
    } -< ()
  -- Download model
  model <- \() -> python {
        src = srcDir,
        entrypoint = [relfile|preload_model.py|],
        args = ["--hub-url", "http://github.com/company/models", "--model-name", "awesomenet"],
        executor = Docker { image = "pytorch/pytorch" }
    } -< ()
  -- Make predictions
  predictions <- \(data, model) -> python {
    src = srcDir,
    entrypoint = [relfile|predict.py|],
    args = ["--dataset", data </> [relfile|dataset.csv|], "--model", model </> [relfile|model.pt|]],
    executor = Docker { image = "pytorch/pytorch", inputs = [data, model] }
    } -< (data, model)
  -- Read predictions
  readFileFromStoreItem [relfile|predictions.txt|] -< predictions


main = print $ runFlow flow ()