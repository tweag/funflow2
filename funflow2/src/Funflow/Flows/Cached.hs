{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - Effects that allows to define flows from pure functions or IO monadic continuations.
 - Those flow will be cached.
 -}
module Funflow.Flows.Cached where

import Control.Kernmantle.Rope ((&), HasKleisli, liftKleisliIO, perform, runReader, strand, untwine, weave')
import Data.ByteString (ByteString)
import qualified Data.CAS.ContentStore as CS
import Data.Default (Default (def))
import qualified Data.Text as T

-- Will tell how to handle writing to the metadata store
type MetadataWriter i o = Maybe (i -> o -> [(T.Text, ByteString)])

-- The set of properties to instantiate a CachedFlow
data CachedFlowProperties i o
  = CachedFlowProperties
      { -- Name of this flow, used when describing the step in diagrams or other reportings.
        name :: Maybe T.Text,
        -- How to cache (or not) the flow.
        cache :: CS.Cacher i o,
        -- Handler to write additional metadata to the content store.
        metadataWriter :: MetadataWriter i o
      }

-- Default value for caching
instance Default (CachedFlowProperties i o) where
  def :: CachedFlowProperties i o
  def =
    CachedFlowProperties
      { name = Nothing,
        cache = CS.NoCache,
        metadataWriter = Nothing
      }

-- interpret the cached effect
runCached :: (HasKleisli IO eff) => CachedFlow a b -> eff a b
runCached command = case command of
  Cached _ f -> liftKleisliIO $ return . f
  CachedIO _ f -> liftKleisliIO f

-- Flows that use caching
data CachedFlow i o where
  -- A flow from a pure function
  Cached :: CachedFlowProperties i o -> (i -> o) -> CachedFlow i o
  -- A flow from a IO monadic continuation
  CachedIO :: CachedFlowProperties i o -> (i -> IO o) -> CachedFlow i o
