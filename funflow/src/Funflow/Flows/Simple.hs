{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

{-
 - "Simple" flows allow to use pure functions or IO monadic continuations
 -}
module Funflow.Flows.Simple where

-- External flows to perform external tasks
data SimpleFlow i o where
  PureFlow :: (i -> o) -> SimpleFlow i o
  IOFlow :: (i -> IO o) -> SimpleFlow i o
