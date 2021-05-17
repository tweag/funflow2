{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
module Funflow.Compat where

import Funflow (caching', caching)
import Funflow.Flow (dockerFlow, getDirFlow, ioFlow, pureFlow, putDirFlow)

stepIO = ioFlow

stepIO' = (flip caching) ioFlow

docker = dockerFlow

putInStore = putDirFlow

getFromStore = getDirFlow
