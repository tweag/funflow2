{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Docker.API.Client.Internal.Client where

import Conduit (filterC, mapC)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson (eitherDecode, encode)
import Data.Binary.Get (getWord32be, getWord8, runGet)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Conduit (ConduitT, await, runConduit, yield, (.|))
import Data.Conduit.Combinators (sinkFile, sinkNull)
import qualified Data.Conduit.Tar as Tar
import Data.Conduit.Zlib (ungzip)
import qualified Data.Text as T
import Docker.API.Client.Internal.Schemas (ContainerId, CreateContainer (..), CreateContainerResponse (..), HostConfig (..), WaitContainerResponse (..))
import Network.HTTP.Client
import Network.HTTP.Client.Internal (makeConnection)
import qualified Network.HTTP.Conduit as HTTPC
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status, status200, status201, status204, statusCode, statusMessage)
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import System.IO (FilePath)
import System.Posix.Types (GroupID, UserID)
import System.PosixCompat.User (getEffectiveGroupID, getEffectiveUserID)

-- | Docker Engine API version. This value will prefix all docker api url paths.
dockerAPIVersion :: String
dockerAPIVersion = "v1.40"

-- | Alias for the system type returned by System.Info.os
type OS = String

-- | Default docker socket path on unix systems
defaultDockerUnixSocket :: FilePath
defaultDockerUnixSocket = "/var/run/docker.sock"

-- | Creates a new HTTP connection manager for the default docker daemon address
-- on your system.
newDefaultDockerManager :: OS -> IO Manager
-- TODO: default uri and manager for windows (which uses tcp instead of a socket)
newDefaultDockerManager "mingw32" = undefined
newDefaultDockerManager _ = newUnixDomainSocketManager defaultDockerUnixSocket

-- | Creates a new http connection manager from a file path to a unix socket
newUnixDomainSocketManager :: FilePath -> IO Manager
newUnixDomainSocketManager path = do
  -- Stolen from: https://kseo.github.io/posts/2017-01-23-custom-connection-manager-for-http-client.html
  let mSettings = defaultManagerSettings {managerRawConnection = return $ openUnixSocket path}
  newManager mSettings
  where
    openUnixSocket filePath _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix filePath)
      makeConnection
        (SBS.recv s 8096)
        (SBS.sendAll s)
        (S.close s)

-- | This type describes common errors the docker client might encounter
data DockerClientError
  = -- | The request to create a new container failed
    ContainerCreationFailedError String
  | -- | The docker engine API responded with something we didn't expect
    UnrecognizedJSONResponseError String
  | -- | The docker engine API responded with an error status code when getting a container archive
    GetContainerArchiveError String
  | -- | The container exited with a nonzero exit code
    NonZeroExitCode String
  | -- | The docker engine API responded with an error when we attempted to get a container's logs
    GetContainerLogsError String
  deriving (Show)

-- | Wrapper for composing operations which return an Either DockerClientError a
type ClientErrorMonad a = ExceptT DockerClientError IO a

-- | Helper function which formats the response of a failed HTTP request
formatRequestError :: Status -> LBS.ByteString -> String
formatRequestError status body =
  "Request failed with status "
    ++ show (statusCode status)
    ++ ": "
    ++ show (statusMessage status)
    ++ " "
    ++ show body

-- | Describes a docker container to be created with runContainer. Use defaultContainerSpec to
-- get a default value.
data ContainerSpec = ContainerSpec
  { image :: T.Text,
    cmd :: [T.Text],
    user :: T.Text,
    workingDir :: T.Text,
    envVars :: [T.Text],
    hostVolumes :: [T.Text]
  }

-- | Constructs a simple default ContainerSpec for a docker image which uses the image's default
-- values for all other aguments.
defaultContainerSpec :: T.Text -> ContainerSpec
defaultContainerSpec img =
  ContainerSpec
    { image = img,
      cmd = [],
      user = "",
      workingDir = "",
      envVars = [],
      hostVolumes = []
    }

-- | (Internal only, do not export) Converts a client ContainerSpec into the format expected by the docker api
containerSpecToCreateContainer :: ContainerSpec -> CreateContainer
containerSpecToCreateContainer spec =
  CreateContainer
    { createContainerUser = textToMaybe $ user spec,
      createContainerWorkingDir = textToMaybe $ workingDir spec,
      createContainerEnv = listToMaybe $ envVars spec,
      createContainerImage = image spec,
      createContainerCmd = listToMaybe $ cmd spec,
      createContainerHostConfig = convertHostVolumes $ hostVolumes spec
    }
  where
    textToMaybe t = if not $ T.null t then Just t else Nothing
    listToMaybe l = if not $ null l then Just l else Nothing
    convertHostVolumes hvs =
      if null hvs
        then Nothing
        else Just HostConfig {hostConfigBinds = Just hvs}

-- | Analagous to the `docker run` command. Runs a container using the input HTTP connection manager and waits
-- until it exits. For more complex use cases you can also use the individual actions that comprise this function.
runContainer :: Manager -> ContainerSpec -> ClientErrorMonad ContainerId
runContainer manager spec = do
  let payload = containerSpecToCreateContainer spec
  cid <- submitCreateContainer manager payload >>= parseCreateContainerResult
  startContainer manager cid >>= submitWaitContainer manager >>= parseWaitContainerResult >>= checkExitStatusCode
  return cid

-- | Analagous to the `docker cp` command. Recursively copies contents at the specified path in the container to
-- the provided output path on the host, setting file permissions to the specified user and group id.  Note that the
-- container must have been started for this to succeed (i.e. it must have a running or finished state).  This method
-- uses conduit to optimize memory usage.
saveContainerArchive :: Manager -> UserID -> GroupID -> FilePath -> FilePath -> ContainerId -> ClientErrorMonad ()
saveContainerArchive manager uid gid itemPath outPath cid = do
  let request =
        setQueryString [("path", Just (B.pack itemPath))] $
          defaultRequest
            { method = "GET",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/archive"
            }
  result <- liftIO $
    runResourceT $ do
      response <- HTTPC.http request manager
      let body = HTTPC.responseBody response
      let status = HTTPC.responseStatus response
      -- Note: we're using a different approach than in the other http requests since the response
      -- is wrapped in a ResourceT when using http-conduit, and we would have to implement
      -- the conduit pipeline using ExceptT to make it compatible with that approach.
      if status == status200
        then do
          -- This operation may throw an unexpected exception. If we want to catch stuff like
          -- tar decoding errors we can switch to Tar.restoreFileIntoLenient
          runConduit $ body .| ungzip .| Tar.untar (Tar.restoreFileInto outPath . chownTarballContent uid gid) .| sinkNull
          return Nothing
        else return $ Just $ GetContainerArchiveError $ formatRequestError status ""
  case result of
    Just e -> throwError e
    Nothing -> return ()

data ContainerLogType = Stdout | StdErr | Both

-- | Conduit helper which checks the length of the input bytestring and reads data from upstream
-- until the length is at least nBytes. Note that this method may return a Bytestring result which
-- has length >= nBytes.
readUpstream :: Monad m => B.ByteString -> Int -> ConduitT B.ByteString o m (Maybe B.ByteString)
readUpstream acc nBytes =
  if B.length acc >= nBytes
    then do return $ Just acc
    else do
      result <- await
      case result of
        Nothing -> return Nothing
        Just val -> readUpstream (B.concat [acc, val]) nBytes

-- | Possible types of streams returned by the Docker Engine API attatch and logs endpoints
data DockerStreamType = StreamStdIn | StreamStdOut | StreamStdErr

-- | Parses the first byte from a stream metadata bytestring returned by the Docker Engine API
-- and returns the corresponding stream type.
getStreamType :: B.ByteString -> DockerStreamType
getStreamType meta
  | indicator == 0 = StreamStdIn
  | indicator == 1 = StreamStdOut
  | indicator == 2 = StreamStdErr
  | otherwise = error "Unrecognized stream type in docker engine response"
  where
    indicator = runGet getWord8 (LBS.fromStrict meta)

-- |
getSectionLength :: B.ByteString -> Int
getSectionLength = fromIntegral . runGet getWord32be . LBS.fromStrict

-- | Parses a docker metadata bytestring of length >= 8 into it's individual components
-- See https://docs.docker.com/engine/api/v1.40/#operation/ContainerAttach
parseDockerStream :: B.ByteString -> (DockerStreamType, Int, B.ByteString)
parseDockerStream bytes =
  let parts = splitToParts bytes
      st = getStreamType $ fst parts
      len = getSectionLength $ fst $ snd parts
      extra = snd $ snd parts
   in (st, len, extra)
  where
    splitToParts b = (B.singleton $ B.head b, B.splitAt 4 $ snd $ B.splitAt 4 b)

-- | Conduit for parsing a multiplexed stream from the Docker Engine API (e.g. the output of the attatch and logs endpoints).
-- This will force memory usage up to the returned frame size (for docker logs this is usually just one line of text).
-- See https://docs.docker.com/engine/api/v1.40/#operation/ContainerAttach for more details on this format.
parseMultiplexedDockerStream :: MonadIO m => ConduitT B.ByteString (DockerStreamType, B.ByteString) m ()
parseMultiplexedDockerStream = loop B.empty
  where
    loop acc = do
      -- This whole thing could be recursive
      -- This ensures that we get at least 8 bytes (could be more)
      input <- readUpstream acc 8
      case input of
        Nothing -> return ()
        Just meta -> do
          let (streamType, sectionLength, extra) = parseDockerStream meta
          -- This ensures that we have at least as much data as our section (could be more)
          section <- readUpstream extra sectionLength
          case section of
            Nothing -> return ()
            Just s -> do
              let (expected, additional) = B.splitAt sectionLength s
              yield (streamType, expected)
              loop additional

-- | Creates a DockerStreamType filter using the input ContainerLogType
createLogTypeFilter :: ContainerLogType -> ((DockerStreamType, a) -> Bool)
createLogTypeFilter clt = case clt of
  Stdout -> \(t, _) ->
    case t of
      StreamStdOut -> True
      _ -> False
  StdErr -> \(t, _) ->
    case t of
      StreamStdErr -> True
      _ -> False
  Both -> \(t, _) ->
    case t of
      _ -> True

-- | Streams the logs from a docker container into the specified output file path. Logs can include
-- stdout, stderr, or both. Note that if you include both streams, the sorting of the timestamps in the output
-- file may not be perfectly sorted since the stream returned by the docker api is only sorted within each
-- stream type (i.e. stdout and stderr are sorted separately).
saveContainerLogs :: Manager -> ContainerLogType -> FilePath -> ContainerId -> ClientErrorMonad ()
saveContainerLogs manager logType outPath cid = do
  let request =
        setQueryString
          [ ("follow", Just "false"),
            ("stdout", Just stdout),
            ("stderr", Just stderr),
            ("timestamps", Just "true")
          ]
          $ defaultRequest
            { method = "GET",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/logs"
            }
        where
          stderr = case logType of
            Stdout -> "false"
            _ -> "true"
          stdout = case logType of
            StdErr -> "false"
            _ -> "true"
  result <- liftIO $
    runResourceT $ do
      response <- HTTPC.http request manager
      let body = HTTPC.responseBody response
      let status = HTTPC.responseStatus response
      -- Note: we're using a different approach than in the other http requests since the response
      -- is wrapped in a ResourceT when using http-conduit, and we would have to implement
      -- the conduit pipeline using ExceptT to make it compatible with that approach.
      if status == status200
        then do
          let isLogType = createLogTypeFilter logType
          runConduit $ body .| parseMultiplexedDockerStream .| filterC isLogType .| mapC snd .| sinkFile outPath
          return Nothing
        else return $ Just $ GetContainerLogsError $ formatRequestError status ""
  case result of
    Just e -> throwError e
    Nothing -> return ()

-- | Attempts to create a docker container, returning the new container's id
submitCreateContainer :: Manager -> CreateContainer -> ClientErrorMonad LBS.ByteString
submitCreateContainer manager object = do
  let reqBody = RequestBodyLBS $ encode object
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/create",
            requestBody = reqBody,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status201 = return body
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

parseCreateContainerResult :: LBS.ByteString -> ClientErrorMonad ContainerId
parseCreateContainerResult body = case eitherDecode body of
  Left msg -> throwError $ UnrecognizedJSONResponseError msg
  Right object -> return $ createContainerResponseId object

startContainer :: Manager -> ContainerId -> ClientErrorMonad ContainerId
startContainer manager cid = do
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/start"
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status204 = return cid
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

submitWaitContainer :: Manager -> ContainerId -> ClientErrorMonad LBS.ByteString
submitWaitContainer manager cid = do
  let request =
        defaultRequest
          { method = "POST",
            path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid ++ "/wait"
          }
  response <- liftIO $ httpLbs request manager
  let result
        -- In the ExceptT world, return is used where you would use Right
        | status == status200 = return $ responseBody response
        -- And throwError is used where you would use Left
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

parseWaitContainerResult :: LBS.ByteString -> ClientErrorMonad WaitContainerResponse
parseWaitContainerResult body = case eitherDecode body of
  Left msg -> throwError $ UnrecognizedJSONResponseError msg
  Right object -> return object

checkExitStatusCode :: WaitContainerResponse -> ClientErrorMonad ()
checkExitStatusCode response = do
  if code == 0
    then return ()
    else case waitContainerResponseError response of
      -- Docker will only sometimes return the associated errors
      Just err -> throwError $ NonZeroExitCode $ show err
      Nothing -> throwError $ NonZeroExitCode $ "Container status code was " ++ show code ++ " with no error message"
  where
    code = waitContainerResponseStatusCode response

removeContainer :: Manager -> Bool -> Bool -> ContainerId -> ClientErrorMonad ContainerId
removeContainer manager isForceful alsoRemoveVolumes cid = do
  let request =
        setQueryString [("force", Just force), ("v", Just v)] $
          defaultRequest
            { method = "DELETE",
              path = B.pack $ "/" ++ dockerAPIVersion ++ "/containers/" ++ cid
            }
        where
          force = if isForceful then "true" else "false"
          v = if alsoRemoveVolumes then "true" else "false"
  response <- liftIO $ httpLbs request manager
  let result
        | status == status204 = return cid
        | otherwise = throwError $ ContainerCreationFailedError $ formatRequestError status body
        where
          body = responseBody response
          status = responseStatus response
  result

-- | Replaces the user and group id for an entry in a tarball with the specified user and group
chownTarballContent :: UserID -> GroupID -> Tar.FileInfo -> Tar.FileInfo
chownTarballContent uid gid info =
  info
    { Tar.fileUserName = "",
      Tar.fileUserId = uid,
      Tar.fileGroupName = "",
      Tar.fileGroupId = gid
    }
