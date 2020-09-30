let
#   jupyter = import (builtins.fetchGit {
#     url = https://github.com/tweag/jupyterWith;
#     rev = "747a461d67b3d56e30c8a988a892ef611c8fe460";
#   }) {};
  jupyter = import /home/dorran/Documents/tweag/jupyterWith {};
  pkgs = import ../nix {};
  
  # Problem: GHC withPackage from haskell.nix doesn't include ihaskell bin executable
  iHaskell = jupyter.kernels.iHaskellWith {
    extraIHaskellFlags = "--codemirror Haskell";  # for jupyterlab syntax highlighting
    name = "haskell";
    customIHaskell = pkgs.symlinkJoin {
      name="ihaskell-hnix"; 
      paths=[
        pkgs.projectHaskellPackages.ihaskell.components.exes.ihaskell
        pkgs.projectHaskellPackages.ihaskell.components.library
        ];
    };
    packages = p: with p; [ funflow hvega ihaskell-hvega];
    haskellPackages = pkgs.projectHaskellPackages;
  };

  jupyterEnvironment =
    jupyter.jupyterlabWith {
      kernels = [ iHaskell ];
    };
in jupyterEnvironment.env
