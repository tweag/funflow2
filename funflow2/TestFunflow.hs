{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Funflow (Flow, caching, ioFlow, pureFlow, runFlow)

main :: IO ()
main = do
  -- Run multiple flows to test funflow's capabilities
  putStr "\n------ TESTING ------\n"
  let somePureFlow :: Flow Int Int
      somePureFlow = pureFlow (+ 1)
  testFlow @Int @Int "a flow from a pure function" somePureFlow 0
  putStr "\n---------------------\n"
  let someIoFlow :: Flow () ()
      someIoFlow = ioFlow $ const $ putStrLn "Some IO operation"
  testFlow @() @() "a flow with IO" someIoFlow ()
  putStr "\n---------------------\n"
  let someCachedFlow :: Flow Int Int
      someCachedFlow = caching "increment" $ pureFlow (+ 1)
  testFlow @Int @Int "a flow from a pure function with caching" someCachedFlow 0
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)
