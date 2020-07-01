let
    pkgs = import ./nixpkgs.nix {};

    base = import ./default.nix;
    base-doc = base.funflow2.components.library.doc;
in
    pkgs.runCommand
        "${base-doc.name}-url-corrected"
        { }
        ''
            cp -R ${base-doc}/* $out
            find $out -exec chmod 755 {} \;
            find $out -type f \
                -exec sed -i -e \
                    "s/file:\/\/\/nix\/store\/[a-z0-9]\+.\+-lib-\(.\+\)-\([0-9\.-]\+\)-haddock-doc\/share\/doc\/\(.\+\)\/html\//https:\/\/hackage.haskell.org\/package\/\1-\2\/docs\//g" \
                    {} \;
        ''