{-# OPTIONS_GHC -F -pgmF inlitpp #-}

```haskell top hide
{-# LANGUAGE OverloadedStrings #-}

import Lib
import Funflow
```

# First steps with `funflow`

## Introduction

`funflow` is a Haskell library to setup tasks in a workflow, a Direct Acyclic Graph (DAG).
These workflows have the great property of being tasks themselves.
This allows to define modular workflows that you can compose together.
Type checking, external docker or nix tasks, task scheduling, caching, and other features that further simplify setting up your machinery.

_Let's get started_

## From "workflows and tasks" to "flows"

In `funflow`, there is no distinction between a workflow and a task.
Indeed, we can compose tasks into bigger tasks, which could also be called workflows.
Since this workflow is just another task, it can be integrated into even bigger workflows.
Because we can no longer make a distinction between a task and a workflow, we simply use the word _flow_.

A flow takes an input and produces an output and `funflow` describes it with a unique and simple type:

```haskell
flow :: Flow input output
```

`input` and `output` are the types of the input and output values of the flow.
For instance a flow working on numbers might have the following type signature:

```haskell
flow :: Flow Int Int
```

It takes an integer as input, and producing an integer as its output.
A flow that doesn't take any input can be written as:

```haskell
flow :: Flow () Int
```

It might, for example request some user input or download some data.

## Effects

Another feature of funflow `Flows` is that they are _effect-controlled_.
In other words, the tasks that you chain together are tagged by what they do.
Here are some examples:

* pure tasks represent pure Haskell functions;
* IO tasks have access to IO;
* docker or nix tasks run an arbitrary commands in an external environment;
* shell tasks execute tasks within the current user environment.

The `Flow` type can assemble all effects or constrain them to specific subsets which is useful to achieve highly reproducible setups.

## How to make flows

The function `toFlow` is used to construct a `Flow`.
It is defined in the module `Funflow.Flow` but also exported in the top level module `Funflow`.
It turns an _effect_ (including pure functions) into a `Flow`.

But what is an effect?
An effect is a datatype that represents some particular type of computation.
The most basic effect, the datatype _PureEffect_, represents a pure function which has no _side effect_ such as reading a file or running a command.
Here is a flow that increments the input by 1.

```haskell
flow :: Flow Int Int
flow = toFlow . PureEffect $ (+1)
```

An effect is not automatically usable as a `Flow`.
We have to _strand_ it into a `Flow`  manually using this `toFlow` function.
And this is where a lot of magic happens because `Flow` can constrain which effects a flow can have while at the same time abstracting over them.
We'll get the notion of _strand_ later when we get to defining custom effects.

#### Smart constructors

All effects that are implemented in `funflow` can be alternatively created using _smart constructors_.
For instance instead of the previous

```haskell
flow :: Flow Int Int
flow = toFlow . PureEffect $ (+1)
```

one can write

```haskell top
flow :: Flow Int Int
flow = pureFlow (+1)
```

this directly makes a flow: the effect is created and _stranded_.

### Execute a flow

Everything needed to run flows is available in the module `Funflow.Run`.
The function `runFlow` is the simples way to execute a flow.
It is used as follow:

```haskell
runFlow flow input
```

where

- `flow` is the `Flow` to run
- `input` is the input, with the same type as the input type of `flow`

It will return a result of type `IO output` where `output` is the output type of `flow`.

Let's run our flow:

```haskell eval
runFlow flow (1 :: Int) :: IO Int
```

As expected, it returned 2.

### Available effects

Available effects are defined in `Funflow.Effects`, and their smart constructors are defined in `Funflow.Flow`
