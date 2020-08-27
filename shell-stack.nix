let
  nixpkgs = import ./nix/default.nix { };
in
nixpkgs.funflow-shell
