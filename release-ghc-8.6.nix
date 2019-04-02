let
  buildPkg = import ./release.nix;
in
  buildPkg {
    channel = "nixos-19.03";
  }
