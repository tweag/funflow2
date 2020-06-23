{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE Arrows #-}
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
    externalFlow,
    ioFlow,
    pureFlow,
  )
-- Required to build a Docker flow
import Funflow.Flows.Docker (DockerFlowConfig (DockerFlowConfig))
import qualified Funflow.Flows.Docker as D
-- Required to build an external flow
import Funflow.Flows.External (ExternalFlowConfig (ExternalFlowConfig))
import qualified Funflow.Flows.External as E

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
  testFlow @() @() "a flow running an external task" someExternalFlow ()
  putStr "\n---------------------\n"
  testFlow @() @() "a flow running an task in docker" someDockerFlow ()
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)

someCachedFlow :: Flow () ()
someCachedFlow = proc () -> do
  () <- caching "someComputation" $ ioFlow (\() -> putStrLn "This message should appear at most once") -< ()
  ioFlow (\() -> putStrLn "If nothing printed, then it works") -< ()

somePureFlow :: Flow Int Int
somePureFlow = pureFlow (+ 1)

someIoFlow :: Flow () ()
someIoFlow = ioFlow $ const $ putStrLn "Some IO operation"

someExternalFlow :: Flow () ()
someExternalFlow = externalFlow (ExternalFlowConfig {E.command = "echo", E.args = ["Hello"], E.env = []})

someDockerFlow :: Flow () ()
someDockerFlow = dockerFlow (DockerFlowConfig {D.image = "python", D.command = "python", D.args = ["-c", "print('Hello')"]})
