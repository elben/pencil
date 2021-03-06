let
  buildPkg = import ./release.nix;
in
  buildPkg {
    channel = "nixos-18.03";

    # Following https://github.com/Gabriel439/haskell-nix/tree/master/project1
    config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            # hsass tests failed. Nix file generated via:
            # cabal2nix --no-check cabal://hsass-0.5.0 > nix/ghc-8.2/hsass-0.5.0.nix
            hsass =
              haskellPackagesNew.callPackage ./nix/ghc-8.2/hsass-0.5.0.nix { };

            # test failing to build
            # cabal2nix --no-check cabal://hlibsass-0.1.6.1 > nix/ghc-8.2/hlibsass-0.1.6.1.nix
            hlibsass =
              haskellPackagesNew.callPackage ./nix/ghc-8.2/hlibsass-0.1.6.1.nix { };

            # Need at least 0.8.31 to get `decodeThrow`
            # cabal2nix cabal://yaml-0.8.32 > nix/ghc-8.2/yaml-0.8.32.nix
            yaml =
              haskellPackagesNew.callPackage ./nix/ghc-8.2/yaml-0.8.32.nix { };
          };
        };
      };
    };
  }
