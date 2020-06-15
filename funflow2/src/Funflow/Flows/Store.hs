{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Funflow.Flows.Store where

import Data.CAS.ContentHashable (ContentHashable)
import Data.CAS.ContentStore as CS
import Path (Abs, Dir, Path)

data DirectStoreAccessFlow i o where
  PutInStore ::
    ContentHashable IO i =>
    (Path Abs Dir -> i -> IO ()) ->
    DirectStoreAccessFlow i CS.Item
  GetFromStore ::
    (Path Abs t -> IO i) ->
    DirectStoreAccessFlow (CS.Content t) i
  -- TODO doc
  InternalManipulateStore ::
    (CS.ContentStore -> i -> IO o) ->
    DirectStoreAccessFlow i o
