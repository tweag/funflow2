{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( -- externalFlow,
    ioFlow,
    pureFlow,
  )

-- import Funflow.Flows.External (ExternalFlowConfig (ExternalFlowConfig))

main :: IO ()
main = do
  -- Run multiple flows to test funflow's capabilities
  putStr "\n------ TESTING ------\n"
  testFlow @Int @Int "a flow from a pure function" somePureFlow 0
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with IO" someIoFlow ()
  putStr "\n---------------------\n"
  testFlow @Int @Int "a flow from a pure function with caching" someCachedFlow 0
  putStr "\n---------------------\n"
  -- testFlow @() @() "a flow running an external task" someExternalFlow ()
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)

someCachedFlow :: Flow Int Int
someCachedFlow = caching "increment" $ pureFlow (+ 1)

somePureFlow :: Flow Int Int
somePureFlow = pureFlow (+ 1)

someIoFlow :: Flow () ()
someIoFlow = ioFlow $ const $ putStrLn "Some IO operation"
-- let someExternalFlow = externalFlow (ExternalFlowConfig {command = "echo", args = ["Hello world"]})
