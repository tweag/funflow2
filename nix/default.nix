{system ? builtins.currentSystem}:
let 
  overlays = import ./overlays.nix;
  pkgs = import ./nixpkgs.nix {overlays=overlays; system=system;};
# TODO - Before release we might want to limit what gets exported below
# to the specific attributes we want to expose.
in pkgs
