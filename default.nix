let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./pencil.nix { }

