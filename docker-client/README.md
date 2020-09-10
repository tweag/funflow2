# docker-client

Docker client library used internally by Funflow.

## Usage

This library exposes a simple API via the Funflow.Docker module.

### Example: Running a container

```haskell
import Funflow.Docker 

import System.Info (os)

main :: IO ()
main = do
  manager <- newDefaultDockerManager os
  uid <- getEffectiveUserID
  gid <- getEffectiveGroupID
  let container =
        (defaultContainerSpec "ubuntu:latest")
          { cmd = ["bash", "-c", "echo foo > /host/test.txt"],
            hostVolumes = ["/home/dorran/Desktop/test-bind:/host"]
          }
  result <- runExceptT $ runContainer manager container
  case result of
    Left err -> print err
    Right cid -> print "Run succeeded! The container is was " ++ cid
```
