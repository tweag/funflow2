{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - The caching effect is already defined in kernmantle
 -}
module Funflow.Flows.Cached where

import Data.Default (Default (def))

-- The set of properties to perform caching
data CachedFlowProperties i o
  = CachedFlowProperties
      { name :: String
      }

-- Default properties for caching
-- TODO proper caching (current is no cache by default)
instance Default (CachedFlowProperties i o) where
  def :: CachedFlowProperties i o
  def =
    CachedFlowProperties
      { name = "unnamed"
      }
