let
  buildPkg = import ./release.nix;
in
  buildPkg {
    channel = "nixos-18.09";

    # Following https://github.com/Gabriel439/haskell-nix/tree/master/project1
    config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            # Since pencil 0.1.3 specifies hsass >= 0.8, and the 18.09 nix
            # channel only has hsass 0.7.
            # cabal2nix cabal://hsass-0.8.0 > hsass-0.8.0.nix
            hsass =
              haskellPackagesNew.callPackage ./nix/ghc-8.4/hsass-0.8.0.nix { };
          };
        };
      };
    };
  }
