
[
    # Overlay for project libraries and executables
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
                { buildInputs = old.buildInputs ++ [super.docker]; }
            );
            funflow-tests = project.funflow.components.tests.test-funflow.overrideAttrs (old:
                { buildInputs = old.buildInputs ++ [super.docker]; }
            );
            
            # Note: Writing these out explicitely incase we add docker or other examples
            # which require an extra buildInput, which can be done by calling overrideAttrs
            # on any of the following:
            funflow-tutorial = {
                quick-reference = project.funflow-tutorial.components.exes.quick-reference;
                tutorial1 = project.funflow-tutorial.components.exes.tutorial1;
                wordcount = project.funflow-tutorial.components.exes.wordcount;
            };

            cas-store = project.cas-store.components.library;
            cas-hashable = project.cas-hashable.components.library;
            cas-hashable-s3 = project.cas-hashable-s3.components.library;
            external-executor = project.external-executor.components.library;

            funflow-shell = project.shellFor ({
                    exactDeps = true;
                    STACK_IN_NIX_SHELL = true;
                }
            );
        }
    )

    # Overlay for script which generates html tutorial docs
    (self: super: 
        {generate-funflow-tutorials = super.callPackage ./tutorials.nix {};}
    )
]
