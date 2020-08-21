{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Funflow.Flow
  ( Flow,
    RequiredStrands,
    RequiredCoreEffects,
    toFlow,
    pureFlow,
    ioFlow,
    dockerFlow,
  )
where

import Control.Arrow (Arrow, ArrowChoice)
import Control.Kernmantle.Caching (ProvidesCaching, StoreWithId (StoreWithId), usingStore)
import Control.Kernmantle.Error (MonadMask)
import Control.Kernmantle.Parallel (PKleisli)
import Control.Kernmantle.Rope (AnyRopeWith, HasKleisli, Reader, mapReader_, mapSieve, strand, type (~>))
import Control.Monad.IO.Class (MonadIO)
import Data.CAS.ContentStore as CS
import Funflow.Effects.Docker (DockerEffect (..), DockerEffectConfig, DockerEffectInput)
import qualified Data.CAS.RemoteCache as Remote
import Funflow.Effects.Simple (SimpleEffect (..))
import UnliftIO (MonadUnliftIO)

-- | The constraints on the set of "strands"
--   These will be "interpreted" into "core effects" (which have contraints defined below).
type RequiredStrands =
  '[ '("simple", SimpleEffect),
     '("docker", DockerEffect)
   ]

-- | The class constraints on the "core effect".
--   The "core effect" is the effect used to run any kind of "binary effect" ("strand")
type RequiredCoreEffects m =
  '[ -- Basic requirement
     Arrow,
     ArrowChoice,
     -- Support IO
     HasKleisli m,
     -- Support caching
     ProvidesCaching
   ]

-- | Flow is the main type of Funflow.
--   It is a task that takes an input value of type `input` and produces an output value of type `output`.
--   It can use any named effect (strand) that is defined in @RequiredStrands@.
type Flow input output =
  forall m.
  (MonadIO m) =>
  AnyRopeWith
    RequiredStrands
    (RequiredCoreEffects m)
    input
    output


-- ** Smart constructors
--
-- Directly make a flow using @IsFlow@'s @toFlow@

-- | Allows to register on which strand a binary effect should be
class IsFlow binEff where
  toFlow :: binEff i o -> Flow i o

instance IsFlow SimpleEffect where
  toFlow = strand #simple

instance IsFlow DockerEffect where
  toFlow = strand #docker

pureFlow :: (i -> o) -> Flow i o
pureFlow = toFlow . PureEffect

ioFlow :: (i -> IO o) -> Flow i o
ioFlow = toFlow . IOEffect

dockerFlow :: DockerEffectConfig -> Flow DockerEffectInput CS.Item
dockerFlow = toFlow . DockerEffect

-- Should be pushed to upstream (kernmantle)
instance
  (MonadIO m, MonadUnliftIO m, MonadMask m, Remote.Cacher m remoteCacher) =>
  ProvidesCaching (Reader (StoreWithId remoteCacher) ~> PKleisli m)
  where
  usingStore =
    mapReader_ $ \(StoreWithId store remoteCacher pipelineId) ->
      mapSieve $ \act input ->
        CS.cacheKleisliIO
          pipelineId
          (CS.defaultIOCacherWithIdent 1)
          store
          remoteCacher
          act
          input