# All overlays for this project should be defined in the list below:
#
[
  # Main project libraries
  (self: super:
    let
      # The `project` local is the project created using haskell.nix. 
      # We extract funflow's libraries from it explicitly below in order to 
      # add extra system dependencies, etc.
      project = super.haskell-nix.stackProject {
        buildInputs = [ super.git ];
        src = super.haskell-nix.haskellLib.cleanGit {
          name = "funflow-project";
          src = ./..;
        };
      };
    in
    {
      projectHaskellPackages = project;
      # TODO - Remove docker buildInput here since it is only needed by the docker-client
      funflow = project.funflow.components.library.overrideAttrs (old:
        { buildInputs = old.buildInputs ++ [ super.docker ]; }
      );
      funflow-tests = project.funflow.components.tests.test-funflow.overrideAttrs (old:
        { buildInputs = old.buildInputs ++ [ super.docker ]; }
      );
      # Note: Writing these components out explicitly incase we add docker or other examples
      # which require an extra buildInput, which can be done by calling overrideAttrs
      # on any of the following:

      project.

      funflow-tutorial = {
        quick-reference = project.funflow-tutorial.components.exes.quick-reference;
        tutorial1 = project.funflow-tutorial.components.exes.tutorial1;
        tutorial2 = project.funflow-tutorial.components.exes.tutorial2;
        wordcount = project.funflow-tutorial.components.exes.wordcount;
      };

      # Shell with funflow's dependencies
      funflow-shell = project.shellFor ({
        exactDeps = true;
        STACK_IN_NIX_SHELL = true;
        buildInputs = [ super.docker ];
      });

      # Other libraries defined in this repo
      cas-store = project.cas-store.components.library;
      cas-hashable = project.cas-hashable.components.library;
      cas-hashable-s3 = project.cas-hashable-s3.components.library;
      external-executor = project.external-executor.components.library;
      docker-client = project.docker-client.components.library.overrideAttrs (old:
        { buildInputs = old.buildInputs ++ [ super.docker ]; }
      );
      docker-client-tests = project.docker-client.components.tests.primary.overrideAttrs (old:
        { buildInputs = old.buildInputs ++ [ super.docker ]; }
      );
    }
  )

  # Need to manually override some stuff in ihaskell to since we are building it via haskell.nix
  # and not via the usual nixpkgs infrastructure. This is mostly stolen from
  # the iHaskell repo: https://github.com/gibiansky/IHaskell/blob/master/release.nix#L36
  (self: super:
    { projectHaskellPackages = super.lib.recursiveUpdate super.projectHaskellPackages
      {
        ihaskell.components.tests.hspec = super.projectHaskellPackages.ihaskell.components.tests.hspec.overrideAttrs
          (old:
            { 
              preCheck = ''
                export HOME=$TMPDIR/home
                export PATH=$PWD/dist/build/ihaskell:$PATH
                export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
              '';
            }
          );
      };
    }
  )
  # Wrapper script for building tutorial html docs
  (self: super:
    { generate-funflow-tutorials = super.callPackage ./pkgs/tutorials.nix { }; }
  )

  # Script for building documentation index.html page
  (self: super:
    { generate-doc-index = super.callPackage ./pkgs/doc-index.nix { }; }
  )

  # Utility function for combining haddock docs into a single closure with 
  # relative hyperlinks (so they work on GitHub pages)
  (self: super:
    { haddock-combine = super.callPackage ./pkgs/haddock-combine.nix { }; }
  )
]
