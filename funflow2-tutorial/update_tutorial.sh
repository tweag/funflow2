#!/bin/sh
#!nix-shell -p stack

echo "# Building tutorials ..."

echo "## Cleaning output folder ..."

rm -rf out 
mkdir -p out

echo "## Generating html files ..."

stack --verbosity silent run funflow2-tutorial:exe:quick-reference > out/quick-reference.html
stack --verbosity silent run funflow2-tutorial:exe:tutorial1 > out/tutorial1.html

echo "## Done."
