let
  buildPkg = import ./release.nix;
in
  buildPkg {
    # Will become nixos-19.03 once released
    channel = "nixos-unstable";
  }
