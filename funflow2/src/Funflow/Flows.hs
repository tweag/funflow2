{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}

{-
 - Utilities to hide the kernmantle API by putting together the user effect and the strand type
 -}
module Funflow.Flows
  ( -- Cached flows
    cachedWithProps,
    cached,
    cachedIOWithProps,
    cachedIO,
  )
where

import Control.Arrow (arr)
import Control.Kernmantle.Caching (caching)
import Control.Kernmantle.Rope (liftKleisliIO)
import Data.CAS.ContentHashable as CH
import Data.Default (def)
import Data.Store (Store)
import Funflow.Base (Flow)
import Funflow.Flows.Cached (CachedFlowProperties (..))

-- TODOs
-- import Funflow.Flows.External
-- import Funflow.Flows.Store

-- Cached flows
cachedWithProps :: (CH.ContentHashable IO i, Store o) => (CachedFlowProperties i o) -> (i -> o) -> Flow i o
cachedWithProps props f =
  let CachedFlowProperties {name} = props
   in caching name (arr f)

cached :: (CH.ContentHashable IO i, Store o) => (i -> o) -> Flow i o
cached = cachedWithProps def

cachedIOWithProps :: (CH.ContentHashable IO i, Store o) => (CachedFlowProperties i o) -> (i -> IO o) -> Flow i o
cachedIOWithProps props f =
  let CachedFlowProperties {name} = props
   in caching name (liftKleisliIO f)

cachedIO :: (CH.ContentHashable IO i, Store o) => (i -> IO o) -> Flow i o
cachedIO = cachedIOWithProps def
