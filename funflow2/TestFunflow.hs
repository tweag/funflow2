{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Kernmantle.Caching (localStoreWithId)
import Control.Kernmantle.Rope ((&), HasKleisli, liftKleisliIO, perform, runReader, strand, untwine, weave')
import qualified Data.CAS.ContentStore as CS
import Data.Default (def)
import Funflow (Flow)
import Funflow.Flows.Cached (CachedFlow (Cached, CachedIO), runCached)
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
flow = strand #cached $ CachedIO def (\() -> putStrLn "Hello")
-- flow :: Flow () ()
-- flow = cachedIO (\() -> putStrLn "Hello")
-- flow = cachedIOWithProps props (\() -> putStrLn "Hello")
