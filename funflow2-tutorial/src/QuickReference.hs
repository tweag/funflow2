{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE NoMonomorphismRestriction #-}

import Lib ()
```

# funflow2: Quick reference

This document presents how to use the API through short examples.

All imports are available in the `Funflow` module:

```haskell top
import Funflow
```

## 1. A minimal flow

```haskell top
-- A flow from a pure function
flow :: Flow [Char] [Char]
flow = pureFlow $ \input -> "Hello " ++ input ++ " !"
-- Some input to run the flow on
input :: [Char]
input = "Watson"
```

```haskell eval
runFlow defaultExecutionConfig flow input :: IO [Char]
```


### 2. Composing flows

```haskell
    let
      -- Two flows
      flow1 = pureFlow $ \input -> "Hello"
      flow2 = pureFlow $ \input -> input ++ " world"
      -- Combine both flows
      flow = flow1 >>> flow2
      -- Empty input
      input = ()
    in
      runFlow defaulExecutionConfig flow input
```

### 3. Running a shell command

```haskell
    let
      -- Prints "Hello world" to stdout and produces no output
      flow = shellFlow "echo Hello world"
    in
      runFlow defaulExecutionConfig flow ()
```

### 4. Caching a flow

```haskell
    let
      -- Prints "Increment!" everytime it runs
      increment = ioFlow $ \input -> do
        putStrLn "Increment!"
        return $ input + 1
      -- Reset to zero
      reset = pureFlow $ \input -> 0
      -- Caching the `increment` flow
      cachedIncrement = caching "increment" increment
      -- Without caching
      flow1 = reset >>> increment >>> reset >>> increment
      -- With caching
      flow2 = reset >>> cachedIncrement >>> reset >>> cachedIncrement
    in
      do
        -- Prints "Increment!" twice to stdout
        runFlow defaulExecutionConfig flow1 ()
        -- Prints "Increment!" once to stdout
        runFlow defaulExecutionConfig flow2 ()
        return ()
```

### Configuration Option

### Controlling Effects

