# This is the main project default.nix
{ system ? builtins.currentSystem
, pkgs ? import ./nix/default.nix { inherit system; } }:
with pkgs; rec {
  # Libraries
  inherit funflow funflow-tests cas-store cas-hashable cas-hashable-s3
    external-executor docker-client docker-client-tests
    api-docs;

  # Shell
  inherit funflow-shell;

  # Tutorial exes (this is a set and each attribute is a funflow-tutorial executable)
  inherit funflow-tutorial;

  # Documentation
  doc-index = generate-doc-index;
  inherit generate-funflow-tutorials;
}
