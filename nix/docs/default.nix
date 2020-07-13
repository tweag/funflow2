let
  pkgs = import ../nixpkgs.nix { };

  docs-api = import ./docs-api.nix;
in pkgs.runCommand "funflow2-docs" { } ''
  mkdir -p $out/docs/api
  cp -R ${docs-api}/doc/funflow2/html/* $out/docs/api
''
