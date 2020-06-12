{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- Common imports
import Funflow
  ( Flow,
    caching,
    runFlow,
  )
-- Standard way of building flows
import Funflow
  ( dockerFlow,
    executorFlow,
    ioFlow,
    nixFlow,
    pureFlow,
    shellFlow,
  )
-- Required to build a Docker flow
import Funflow.Flows.Docker (DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as D
-- Required to build an executor flow
import Funflow.Flows.Executor (ExecutorFlowConfig (ExecutorFlowConfig))
import qualified Funflow.Flows.Executor as E
import qualified Funflow.Flows.Nix as N

main :: IO ()
main = do
  -- Run multiple flows to test funflow's capabilities
  putStr "\n------ TESTING ------\n"
  testFlow @Int @Int "a flow from a pure function" somePureFlow 0
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with IO" someIoFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with caching" someCachedFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a command in shell" someShellFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running an executor task" someExecutorFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in docker" someDockerFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in a nix shell" someNixFlow ()
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)

someCachedFlow :: Flow () ()
someCachedFlow = proc () -> do
  () <- caching ("someComputation" :: String) $ ioFlow (\() -> putStrLn "This message should appear at most once") -< ()
  () <- caching ("someComputation" :: String) $ ioFlow (\() -> putStrLn "This message should appear at most once") -< ()
  ioFlow (\() -> putStrLn "If nothing printed, then it works") -< ()

somePureFlow :: Flow Int Int
somePureFlow = pureFlow (+ 1)

someIoFlow :: Flow () ()
someIoFlow = ioFlow $ const $ putStrLn "Some IO operation"

someShellFlow :: Flow () ()
someShellFlow = shellFlow "echo If this prints, then shell flow works"

someExecutorFlow :: Flow () ()
someExecutorFlow = executorFlow (ExecutorFlowConfig {E.command = "echo", E.args = ["Hello"], E.env = []})

someDockerFlow :: Flow () ()
someDockerFlow = dockerFlow (DockerFlowConfig {D.image = "python", D.command = "python", D.args = ["-c", "print('Hello')"]})

someNixFlow :: Flow () ()
someNixFlow = nixFlow (N.NixFlowConfig {N.nixEnv = N.PackageList ["python"], N.command = "python -c \"print('Hello')\"", N.args = [], N.env = [], N.nixpkgsSource = N.NIX_PATH})
