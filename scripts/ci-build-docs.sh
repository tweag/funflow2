#!/usr/bin/env bash

set -o errexit

echo "Building API docs"
stack build --nix --haddock

echo "Building Tutorials"
nix-build ./nix -A generate-funflow-tutorials -o result

echo "Preparing API docs for output"
# Get the path to the docs
line="$(stack path | grep 'local-doc-root')"
docs_dir=${line#'local-doc-root: '}

mkdir -p ./funflow-docs/docs
ln -s "${docs_dir}" ./funflow-docs/docs/api

echo "Preparing tutorial docs for output"
cp -r ./result/docs/tutorial ./funflow-docs/docs
