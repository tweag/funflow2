
[
    # Main project libraries
    (self: super:
        let
            project = super.haskell-nix.stackProject {
                    src = super.haskell-nix.haskellLib.cleanGit {
                        name = "funflow-project";
                        src = ./..;
                    };
            };
        in
        {
            funflow = project.funflow.components.library.overrideAttrs (old:
                { buildInputs = old.buildInputs ++ [super.docker];}
            );
            funflow-tests = project.funflow.components.tests.test-funflow.overrideAttrs (old:
                { buildInputs = old.buildInputs ++ [super.docker]; }
            );   
            # Note: Writing these components out explicitly incase we add docker or other examples
            # which require an extra buildInput, which can be done by calling overrideAttrs
            # on any of the following:
            funflow-tutorial = {
                quick-reference = project.funflow-tutorial.components.exes.quick-reference;
                tutorial1 = project.funflow-tutorial.components.exes.tutorial1;
                wordcount = project.funflow-tutorial.components.exes.wordcount;
            };

            # Shell with funflow's dependencies
            funflow-shell = project.shellFor ({
                    exactDeps = true;
                    STACK_IN_NIX_SHELL = true;
                }
            );

            # Other libraries defined in this repo
            cas-store = project.cas-store.components.library;
            cas-hashable = project.cas-hashable.components.library;
            cas-hashable-s3 = project.cas-hashable-s3.components.library;
            external-executor = project.external-executor.components.library;
        }
    )

    (self: super: 
        {generate-funflow-tutorials = super.callPackage ./pkgs/tutorials.nix {};}
    )

    (self: super: 
        {haddock-combine = super.callPackage ./pkgs/haddock-combine.nix {};}
    )
]
