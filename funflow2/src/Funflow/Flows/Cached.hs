{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - Effects that allows to define flows that are cached from pure functions or IO monadic continuations.
 -}
module Funflow.Flows.Cached where

import Control.Kernmantle.Rope (HasKleisli, liftKleisliIO)
import Data.ByteString (ByteString)
import qualified Data.CAS.ContentStore as CS
import Data.Default (Default (def))
import qualified Data.Text as T

-- Tell how to handle writing to the metadata store
type MetadataWriter i o = Maybe (i -> o -> [(T.Text, ByteString)])

-- The set of properties to perform caching
data CachedFlowProperties i o
  = CachedFlowProperties
      { -- How to cache (or not) the flow.
        cache :: CS.Cacher i o
      }

-- Default properties for caching
-- TODO proper caching (current is no cache by default)
instance Default (CachedFlowProperties i o) where
  def :: CachedFlowProperties i o
  def =
    CachedFlowProperties
      { cache = CS.NoCache
      }

-- Flows that use caching
data CachedFlow i o where
  -- Cache a pure function
  Cached :: CachedFlowProperties i o -> (i -> o) -> CachedFlow i o
  -- Cache a IO monadic continuation
  CachedIO :: CachedFlowProperties i o -> (i -> IO o) -> CachedFlow i o

-- Interpret the cached effect
-- TODO use proper caching (current is dummy)
runCached :: (HasKleisli IO eff) => CachedFlow a b -> eff a b
runCached command = case command of
  Cached _ f -> liftKleisliIO $ return . f
  CachedIO _ f -> liftKleisliIO f
