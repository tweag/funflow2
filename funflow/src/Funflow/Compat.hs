module Funflow.Compat where

import Funflow (caching)
import Funflow.Flow (dockerFlow, getDirFlow, ioFlow, pureFlow, putDirFlow)

stepIO = ioFlow

stepIO' = flip caching ioFlow

docker = dockerFlow

putInStore = putDirFlow

getFromStore = getDirFlow
