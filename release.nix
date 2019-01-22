# Based off https://gist.github.com/mayhewluke/e2f67c65c2e135f16a8e
#
#
# Args:
#
# - channel - A channel string (see http://howoldis.herokuapp.com). Used to check for the channel's latest commit hash
#   (every N days). That way, we can make sure that we are testing with a recent version of the channel. Allows us to
#   pass in different channels, so that we can test different versions of GHC.
#
# - config - An attribute set that is inherited into pkgs. Useful for, say, overriding certain Haskell packages.
#
{ channel, config ? {} }:
let
  sysPkgs = import <nixpkgs> { };

  # See runCommand here: https://github.com/NixOS/nixpkgs/blob/master/pkgs/build-support/trivial-builders.nix#L5
  # The name passed in, "latestRevision", becomes the derivation name. The variable `latestRevision` will refer to
  # the built derivation, whose content is the hash string.
  latestRevision = import (sysPkgs.runCommand "latestRevision"
    {
      buildInputs = [ sysPkgs.wget ];

      # Use this to change the input so that we check the revision every 14 days.
      dummy = builtins.currentTime / (60 * 60 * 24 * 14);
    }
    ''
    # Get the latest git hash for this channel, and save that hash into the $out file that will be specified
    # by Nix's mkDerivation function.
    #
    # See http://howoldis.herokuapp.com for the latest hashes for each release. We want the latest hash so that
    # we will (hopefully) use commits that have been built by Hydra, and have their binaries cached.

    wget --no-check-certificate -O - https://nixos.org/channels/${channel}/git-revision |\
     sed 's#\(.*\)#"\1"#' > $out
    ''
  );

  pkgs = import
            (builtins.fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/${latestRevision}.tar.gz")
            { inherit config; };
in
  pkgs.haskellPackages.callPackage ./pencil.nix { }

