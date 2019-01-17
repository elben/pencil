# Based off https://gist.github.com/mayhewluke/e2f67c65c2e135f16a8e
channel:
let
  sysPkgs = import <nixpkgs> { };

  # See runCommand here: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders.nix#L5
  # The name passed in, "latestRevision", becomes the derivation name. The variable `latestRevision` will refer to
  # the built derivation, whose content is the hash string.
  latestRevision = import (sysPkgs.runCommand "latestRevision"
    {
      buildInputs = [ sysPkgs.git sysPkgs.wget ];
      dummy = builtins.currentTime;
    }
    ''
    # https://nixos.org/channels/nixos-18.09/git-revision
    
    
    # Get the latest git hash for this channel, and save that hash into the $out file that will be specified
    # by Nix's mkDerivation function.
    #
    # See http://howoldis.herokuapp.com for the latest hashes for each release. We want the latest hash so that
    # we will (hopefully) use commits that have been built by Hydra, and have their binaries cached.
    # git ls-remote git://github.com/nixos/nixpkgs.git refs/heads/${channel} | awk '{print "\"" $1 "\""}' > $out
    wget --no-check-certificate -O - https://nixos.org/channels/${channel}/git-revision |\
     sed 's#\(.*\)#"\1"#' > $out
    ''
  );
  pkgs = import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/${latestRevision}.tar.gz") {};
in
  pkgs.haskellPackages.callPackage ./pencil.nix { }

