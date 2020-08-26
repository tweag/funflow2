#!/usr/bin/env bash

set -o errexit

echo "Building API docs"
# Note: this is a quick and dirty way to get haddock docks with relative links
# since unfortunately the docs produced via haskell.nix have absolute paths
# which won't work on GitHub pages.
nix-shell -p stack --command 'stack build --nix --haddock'

echo "Building Tutorials"
nix-build ./nix -A generate-funflow-tutorials -o result

echo "Preparing API docs for output"
# Get the path to the docs
line="$(nix-shell -p stack --command 'stack path' | grep 'local-doc-root')"
docs_dir=${line#'local-doc-root: '}

mkdir -p ./funflow-docs/docs
ln -s "${docs_dir}" ./funflow-docs/docs/api

echo "Preparing tutorial docs for output"
cp -r ./result/docs/tutorial ./funflow-docs/docs
