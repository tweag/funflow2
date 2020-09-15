module Docker.API.Client
  ( dockerAPIVersion,
    OS,
    defaultDockerUnixSocket,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
    DockerClientError,
    ClientErrorMonad,
    ContainerSpec (..),
    defaultContainerSpec,
    runContainer,
    saveContainerArchive,
    removeContainer,
    saveContainerLogs,
    ContainerLogType (..),
  )
where

import Docker.API.Client.Internal.Client
  ( ClientErrorMonad,
    ContainerLogType (..),
    ContainerSpec (..),
    DockerClientError,
    OS,
    defaultContainerSpec,
    defaultDockerUnixSocket,
    dockerAPIVersion,
    newDefaultDockerManager,
    newUnixDomainSocketManager,
    removeContainer,
    runContainer,
    saveContainerArchive,
    saveContainerLogs,
  )
