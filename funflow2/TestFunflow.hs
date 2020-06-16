{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope ((&), perform, runReader, untwine, weave')
import qualified Data.CAS.ContentStore as CS
import Funflow.Base (Flow)
import Funflow.Flows (cachedIO)
import Funflow.Flows.Cached (runCached)
import Path (Abs, Dir, absdir)

main :: IO ()
main =
  CS.withStore [absdir|/tmp/_store|] $ \store -> do
    flow
      & weave' #cached runCached
      & untwine
      & runReader (localStoreWithId store $ Just 1)
      & perform ()

-- main :: IO ()
-- main = print $ runFlow flow input
-- main = print $ runFlowWithProps props flow input

flow :: Flow () ()
flow = cachedIO (\() -> putStr "\n\n====================\nIT WORKS\n====================\n\n")
