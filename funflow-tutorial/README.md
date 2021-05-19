# Funflow tutorial

This folder holds the tutorials for `funflow`. They are written as a series of Jupyter notebooks.

## About

The tutorials are meant to progressively teach new users how to use the library and tools.
Tutorials are published as a part of the `funflow` documentation. They can be run locally using
the Jupyter Lab environment and IHaskell kernel defined in [shell.nix](./shell.nix):

```console
$ nix-shell
$ jupyter lab
```

## Development

The tutorials are written in IPython notebook files in the [notebooks/](./notebooks) directory. Any
IHaskell notebook stored there will be included with the `funflow` documentation. Tutorials
with additional supporting files should be placed in a subdirectory, e.g. `notebooks/WordCount/WordCount.ipynb`. 

## Establish Docker

Some tutorials require that Docker is up and running on your machine. Check out the official Docker website for more about [installation](https://docs.docker.com/engine/install/), and perhaps about the [Docker daemon](https://docs.docker.com/config/daemon/).
