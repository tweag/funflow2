{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Implementation of filesystem watching functionality for linux based on
--   inotify.
module Data.CAS.ContentStore.Notify.Linux
  ( Notifier,
    initNotifier,
    killNotifier,
    Watch,
    addDirWatch,
    removeDirWatch,
  )
where

import Control.Exception.Safe (catch)
#if MIN_VERSION_hinotify(0,3,10)
import qualified Data.ByteString.Char8 as BS
#endif
import System.INotify

type Notifier = INotify

initNotifier :: IO Notifier
initNotifier = initINotify

killNotifier :: Notifier -> IO ()
killNotifier = killINotify

type Watch = WatchDescriptor

addDirWatch :: Notifier -> FilePath -> IO () -> IO Watch
addDirWatch inotify dir f = addWatch inotify mask dir' $ \case
  Attributes True Nothing -> f
  MovedSelf True -> f
  DeletedSelf -> f
  _ -> return ()
  where
    mask = [Attrib, MoveSelf, DeleteSelf, OnlyDir]

#if MIN_VERSION_hinotify(0,3,10)
    dir' = BS.pack dir
#else
    dir' = dir
#endif

removeDirWatch :: Watch -> IO ()
removeDirWatch w =
  -- When calling `addWatch` on a path that is already being watched,
  -- inotify will not create a new watch, but amend the existing watch
  -- and return the same watch descriptor.
  -- Therefore, the watch might already have been removed at this point,
  -- which will cause an 'IOError'.
  -- Fortunately, all event handlers to a file are called at once.
  -- So, that removing the watch here will not cause another handler
  -- to miss out on the event.
  -- Note, that this may change when adding different event handlers,
  -- that remove the watch under different conditions.
  removeWatch w
    `catch` \(_ :: IOError) -> return ()
