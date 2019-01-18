let
  buildPkg = import ./release.nix;
in
  buildPkg {
    channel = "nixos-18.09";
  }
