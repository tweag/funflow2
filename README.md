# funflow2

Compose and run computational workflows.

> **Disclaimer**: funflow2 is in an extremely early state and is subject to change. Use at your own risk.

## Introduction

`funflow2` is a Haskell library to write workflows programatically. It is the spiritual
successor to the excellent [funflow](https://github.com/tweag/funflow) library and builds upon
the [kernmantle](https://github.com/tweag/kernmantle/) binary effects library.

Funflow allows you to compose tasks to make *reusable* workflows and helps promote reproducibility
by employing a content-addressed store.

https://tweag.github.io/funflow2/

## Getting started

### Installation

While funflow2 hasn't been plublished on Hackage yet, you can install it as a git dependency using the standard Haskell tooling (e.g. [Stack](https://docs.haskellstack.org/en/stable/yaml_configuration/#packages)). Note that you will
also need to add a couple of extra dependencies to your `stack.yaml`. See the [example stack.yaml](./docs/example-user-stack.yaml) for a minimal example.

### Tutorials

Check out the tutorials on the funflow2 website to get started: https://tweag.github.io/funflow2/tutorials/.

You can run the tutorial notebooks using the nix shell provided in the [funflow-tutorial](./funflow-tutorial) directory.

## Documentation

The API documentation can be found here: https://tweag.github.io/funflow2/api/

## Developement

For those interested in contributing, please see [DEVELOPMENT.md](./docs/DEVELOPMENT.md).
