{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import qualified Data.CAS.ContentStore as CS
import Funflow
  ( CommandExecutionHandler (SystemExecutor),
    Flow,
    FlowExecutionConfig (FlowExecutionConfig),
    caching,
    commandExecution,
    commandFlow,
    dockerFlow,
    ioFlow,
    nixFlow,
    pureFlow,
    runFlow,
    shellFlow,
  )
import Funflow.Flows.Command (CommandFlowConfig (CommandFlowConfig), CommandFlowInput (CommandFlowInput))
import qualified Funflow.Flows.Command as CF
import Funflow.Flows.Docker (DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as DF
import qualified Funflow.Flows.Nix as NF
import Funflow.Util (Optional (Empty))

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
  testFlow @() @() "a flow running a shell string command" someShellFlow ()
  putStr "\n---------------------\n"
  testFlow @() @CS.Item "a flow running a command" someCommandFlow ()
  putStr "\n---------------------\n"
  testFlow @CommandFlowInput @CS.Item "a flow running a task in docker" someDockerFlow (CF.CommandFlowInput {CF.workingDirectoryContent = Empty})
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running a task in a nix shell" someNixFlow ()
  putStr "\n------  DONE   ------\n"

testFlowExecutionConfig :: FlowExecutionConfig
testFlowExecutionConfig = FlowExecutionConfig {commandExecution = SystemExecutor}

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o testFlowExecutionConfig flow input
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
someShellFlow = shellFlow "echo someShellFlow worked"

someCommandFlow :: Flow () CS.Item
someCommandFlow = proc () -> do
  commandFlow (CommandFlowConfig {CF.command = "echo", CF.args = ["someCommandFlow worked"], CF.env = []}) -< CommandFlowInput {CF.workingDirectoryContent = Empty}

someDockerFlow :: Flow CommandFlowInput CS.Item
someDockerFlow = dockerFlow (DockerFlowConfig {DF.image = "python"}) (CommandFlowConfig {CF.command = "python", CF.args = ["-c", "print('someDockerFlow worked')"], CF.env = []})

someNixFlow :: Flow () ()
someNixFlow = nixFlow (NF.NixFlowConfig {NF.nixEnv = NF.PackageList ["python"], NF.command = "python -c \"print('someNixFlow worked')\"", NF.args = [], NF.env = [], NF.nixpkgsSource = NF.NIX_PATH})
