{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Funflow.Tasks.Config (ConfigTask (..)) where

import Path (Abs, File, Path)

data ConfigTask i o where
  -- Given a file path, define a config task. Make this run in the IO monad to make
  -- it less of a pain in the ass to configure the config file path at
  -- runtime
  FileConfigTask :: IO (Path Abs File) -> ConfigTask () o
