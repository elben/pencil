#! /bin/sh
# To debug problems, change the hie-wrapper command below to capture logs like
# this: hie-wrapper ... -c ~/Desktop/hie-session
#
# $@ to pass whatever arguments our IDE may want hie-wrapper to receive.
nix-shell --attr env default.nix --run "hie-wrapper $@"
