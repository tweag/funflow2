module Funflow.Docker
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

import Funflow.Docker.Internal.Client
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
