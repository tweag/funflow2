module Docker.API.Client
  ( dockerAPIVersion,
    OS (..),
    defaultDockerUnixSocket,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
    DockerClientError (..),
    ClientErrorMonad,
    ContainerSpec (..),
    defaultContainerSpec,
    runContainer,
    saveContainerArchive,
    removeContainer,
    saveContainerLogs,
    ContainerLogType (..),
    pullImage,
    awaitContainer,
    tagImageIfMissing,
    updateImageDigest,
    updateImageName,
    updateImageTag,
  )
where

import Docker.API.Client.Images
  ( tagImageIfMissing,
    updateImageDigest,
    updateImageName,
    updateImageTag,
  )
import Docker.API.Client.Internal.Connection
  ( defaultDockerUnixSocket,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
  )
import Docker.API.Client.Internal.Requests
  ( awaitContainer,
    dockerAPIVersion,
    pullImage,
    removeContainer,
    runContainer,
    saveContainerArchive,
    saveContainerLogs,
  )
import Docker.API.Client.Internal.Types
  ( ClientErrorMonad,
    ContainerLogType (..),
    ContainerSpec (..),
    DockerClientError (..),
    OS (..),
    defaultContainerSpec,
  )
