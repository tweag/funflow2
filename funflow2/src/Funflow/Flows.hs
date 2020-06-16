{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

{-
 - Utilities to hide the kernmantle API
 -}
module Funflow.Flows
  ( -- Cached flows
    cachedWithProps,
    cached,
    cachedIOWithProps,
    cachedIO,
  )
where

import Control.Kernmantle.Rope (strand)
import Data.Default (def)
import Funflow.Base (Flow)
import Funflow.Flows.Cached (CachedFlow (Cached, CachedIO), CachedFlowProperties)

-- TODOs
-- import Funflow.Flows.External
-- import Funflow.Flows.Store

-- Cached flows
cachedWithProps :: (CachedFlowProperties i o) -> (i -> o) -> Flow i o
cachedWithProps props f = strand #cached $ Cached props f

cached :: (i -> o) -> Flow i o
cached = cachedWithProps def

cachedIOWithProps :: (CachedFlowProperties i o) -> (i -> IO o) -> Flow i o
cachedIOWithProps props f = strand #cached $ CachedIO props f

cachedIO :: (i -> IO o) -> Flow i o
cachedIO = cachedIOWithProps def
