{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import Funflow (Flow, caching, ioFlow, pureFlow, runFlow)

main :: IO ()
main = do
  putStr "\n------ TESTING ------\n"
  -- Run a simple flow from a pure function
  testFlow @Int @Int "a flow from a pure function" (pureFlow (+ 1)) 0
  putStr "\n---------------------\n"
  testFlow @() @() "a flow with IO" (ioFlow (const $ putStrLn "Some IO operation")) ()
  putStr "\n---------------------\n"
  testFlow @Int @Int "a flow from a pure function with caching" (caching "increment" $ pureFlow (+ 1)) 0
  putStr "\n------  DONE   ------\n"

testFlow :: forall i o. (Show i, Show o) => String -> Flow i o -> i -> IO ()
testFlow label flow input = do
  putStrLn $ "Testing " ++ label
  result <- runFlow @i @o flow input
  putStrLn $ "Got " ++ (show result) ++ " from input " ++ (show input)
