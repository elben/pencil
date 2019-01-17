let
  buildPkg = import ./release.nix;
in
  buildPkg "nixos-18.03"
