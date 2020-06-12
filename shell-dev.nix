{ nixpkgs ? <nixpkgs>,
  ghcide ? true,
  python-language-server ? true,
}:
let
  pkgs = import nixpkgs { };

  # Fetch from GitHub
  devShellsInputs =
    import
      (builtins.fetchGit {
        url = "git@github.com:tweag/nix-dev-shells.git";
        name = "nix-dev-shell";
        rev = "97c533d9e38cb4d32a68449cbfc2f90af99c94fb";
      })
    # Use your own version of nixpkgs
    { inherit pkgs; }
  ;

in
  pkgs.mkShell {
    buildInputs = with devShellsInputs;
      # Common packages (e.g. tmate, git, ...)
      ( common { } )
      # Vim
      ++ ( vim { languageClient = true; languageClientOptions = { inherit ghcide python-language-server; }; } )
      # Standard Haskell dev environment
      ++
        (
          haskell
            {
              inherit ghcide;
              ghcideVersionSelector = ghcidePkgs: ghcidePkgs.ghcide-ghc883;
            }
        )
      # Python dev environment with packages
      ++
        (
          python
            {
              withPackages =
                pp:
                  [
                    # Needs fixing
                    # pp.pytorchWithoutCuda
                    # pp.torchvision
                  ]
              ;
            }
        )
      # Custom
      ++
        [
          pkgs.cabal-install
        ]
    ;
    
    NIX_PATH="nixpkgs=${nixpkgs}";
  }
