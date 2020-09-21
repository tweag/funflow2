{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Funflow
```

# First steps with `funflow`

## Introduction

`funflow` is a Haskell library for defining and running _workflows_.
A workflow specifies a pipeline of _tasks_ structured in a Direct Acyclic Graph (DAG).
Workflows in `funflow` have the great property of being composable which means that  you can easily share and combine components across different workflows.
It supports type checking, result caching, and other features that simplify setting up your machinery.

_Let's get started_

## Anatomy of a Flow

In `funflow`, we refer to workflows as `flows`.
A `Flow` takes an input and produces an  output, and `funflow` describes it with a unique and simple type:

```haskell
flow :: Flow input output
```

`input` and `output` are the types of the input and output values of the flow.
For instance a flow working on numbers might have the following type signature:

```haskell
flow :: Flow Int Int
```

It takes an integer as input and produces an integer as its output.
A flow that doesn't take any input can be written as:

```haskell
flow :: Flow () Int
```

Such a flow might request some user input or download some data.

## Tasks

A `Flow` is a DAG comprising one or more `Tasks` which describe __what__ you would like to execute. 
`funflow` works with a wide range of task granularities.
A `Task` can be a simple Haskell function, a database query, a command to run in a Docker container, or more. 

There are several different types of tasks in Funflow, each describing a specific type of computation. 
Tasks are defined in the `Funflow.Tasks` subpackage.
The most basic task, the datatype `PureTask`, represents  a pure Haskell function which has no _side effects_ such as reading a file or running a command.


## How to create Flows

The function `toFlow` is used to construct a `Flow`.
It can be imported from the top level `Funflow` module and is defined in `Funflow.Flow`.
It integrates a `Task` into a `Flow` which can then be composed with other flows into a larger, final `Flow`.

Here is a `Flow` that runs a `PureTask`, incrementing its input by 1.

```haskell
flow :: Flow Int Int
flow = toFlow $ PureTask (+1)
```

In this example, `flow` is essentially a DAG with one node, `PureTask (+1)``. 
And here is a flow that runs a simple IO task.

```
flow :: Flow String ()
flow = toFlow $ IOTask putStrLn
```

One-node `Flows` with a specific task type as the ones above can also be created directly.
For instance, instead of the previous, one can write:

```haskell top
flow :: Flow Int Int
flow = pureFlow (+1)
```

or

```haskell top
flow :: Flow String ()
flow = ioFlow putStrLn
```

Smart constuctors for other task types are defined in `Funflow.Flow`.

## Execute a flow

Everything needed to run flows is available in the module `Funflow.Run`.
The function `runFlow` is the simplest way to execute a flow:

```haskell
runFlow flow input
```

where

- `flow` is the `Flow` to run
- `input` is the input, with the same type as the input type of `flow`

It will return a result of type `IO output` where `output` is the output type of `flow`.
Why is it an `IO output`?
Let's run our flow from earlier:

```haskell eval
runFlow flow (1 :: Int) :: IO Int
```

As expected, it returned 2.

## Tasks, Effects and Interpretation

In technical terms, funflow `Tasks` describe a specific type of computation and can be seen as _computational effects_.
Tasks in funflow are then _interpreted_ at execution time.
TODO: bring in load time.
This means that we can separately specify __how__ a Task description is transformed into executable code.
This means that the `Tasks` that you chain together are tagged by what they do.
The `Flow` type can assemble all effects or constrain them to specific subsets which is useful to achieve highly reproducible workflows. 

executed (i.e. its interpreter). The easiest way to get started with `funflow` is to 
use the default set of task interpreters that it ships with, which is what the `runFlow` function 
is doing above. Take a look at our [TODO developer documentation](TODO) to learn more about writing custom 
interpreters.

