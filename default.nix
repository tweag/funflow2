{ system ? builtins.currentSystem
, pkgs ? import ./nix/default.nix { inherit system; }
}:
let
  # These libraries get bundled into the API documentation
  doc-libs = with pkgs; [
    funflow
    funflow-tests
    cas-store
    cas-hashable
    cas-hashable-s3
    external-executor
  ];
in
with pkgs; rec {
  # Libraries
  inherit funflow funflow-tests cas-store cas-hashable cas-hashable-s3 external-executor;

  # Shell
  inherit funflow-shell;

  # Tutorial exes (this is a set)
  inherit funflow-tutorial;

  # Documentation
  api-docs = haddock-combine { hspkgs = doc-libs; };
  tutorial-docs = pkgs.generate-funflow-tutorials;
  combined-docs = pkgs.symlinkJoin { name = "funflow-combined-docs"; paths = [ api-docs pkgs.generate-funflow-tutorials ]; };
}
