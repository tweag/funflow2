#! /usr/bin/env nix-shell
#! nix-shell -i bash -p haskell.packages.ghc883.ormolu
find funflow/ -name "*.hs" -exec ormolu --mode inplace {} \;