{-# OPTIONS_GHC -F -pgmF inlitpp #-}

# Handling failures in pipelines with funflow

A pipeline often comprises tasks that can fail.
It is important to prevent such failures as early as possible, like funflow tries to do, but it does not forbid from preparing for it.

In this tutorial, we will present how funflow allows to handle failure from tasks.

## Requirements

This tutorial will use the following language extensions:

```haskell top
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
```

```haskell top hide
import Lib
```

and imports:

```haskell top
import Control.Exception.Safe (SomeException)
import qualified Data.CAS.ContentStore as CS
import Funflow (Flow, pureFlow, ioFlow, dockerFlow, throwStringFlow, returnFlow, tryE)
import Funflow.Tasks.Docker (DockerTaskConfig (DockerTaskConfig), DockerTaskInput (DockerTaskInput), args, argsVals, command, image, inputBindings, Arg(Placeholder))
```

## Handle a task that can fail

Let's imagine that we write a task that can fail, such as a Docker task:

```haskell top
someFlowThatFails :: Flow DockerTaskInput CS.Item
someFlowThatFails = dockerFlow (DockerTaskConfig {image = "badImageName", command = "badCommand", args = [Placeholder "missingArgument"]})
```

this flow can possibly fail for different reasons:
- the image name does not exist or cannot be pulled
- the command does not exist
- the command exists with an error code
- one of the placeholder arguments is not filled

> More about that in the API documentation of the Docker task.

If we were to directly run this flow, the workflow would terminate as soon as an exception would be thrown.

In order to handle the exception, we have to use funflow's `tryE` function.
This function transforms a `Flow i o` that can throw an exception of type `ex` into a flow of type `Flow i (Either ex o)`.
This means that the result of the flow is either the exception on the left or the result on the right.

```haskell top
flow :: Flow () String
flow = proc () -> do
  -- Try to run a flow that fails, receive the result in an @Either SomeException ()@
  result <- tryE @SomeException someFlowThatFails -< DockerTaskInput {inputBindings = mempty, argsVals = mempty}
  case result of
    Left _ ->
      returnFlow -< "The task failed"
    Right _ ->
      returnFlow -< "The task succeeded"
```

In this example, we run the docker task, but we wrap it with the `tryE` function in order to receive the exception if any is thrown.
We can then decide on how the rest of the pipeline behaves after the task, even if it failed.
We could for instance log the exception to a file or realise cleaning operations.

Note that we had to specify which type of exception we were excepting to have when writing `(result :: Either SomeException CS.Item)`.
This requires the `ScopedTypeVariables` extension.

## Throwing exceptions in a workflow

The implementation of tasks will most often throw an exception whenever something has failed.
However, sometimes, based on the result of a computation, you want to decide that a task has failed.

In funflow, it is possible for you to throw an exception manually, using `throwStringFlow`:

```haskell top
flowThatFails :: Flow () ()
flowThatFails = proc () -> do
  -- Do some computations first (this is a dummy example)
  result <- pureFlow id -< ()
  -- Depending on the result, throw an exception
  if result == ()
    then throwStringFlow -< "Nothing has been done (as expected for this example)"
    else returnFlow -< ()
```

you can then handle the failure as previously demonstrated:

```haskell top
flow' :: Flow () ()
flow' = proc () -> do
  -- Try to run a flow that fails, receive the result in an @Either SomeException ()@
  result <- tryE @SomeException flowThatFails -< ()
  -- Handle result as previously shown
  case result of
    Left exception ->
      (ioFlow $ \exception -> putStrLn $ "Exception caught: " ++ show exception) -< exception
    Right () ->
      ioFlow $ const $ error "Exception not caught" -< ()
```