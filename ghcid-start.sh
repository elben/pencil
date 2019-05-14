#nix-shell --attr env --run "ghcid \"--command=cabal exec -- ghci :l Pencil\""
#nix-shell --attr env --run "ghcid \"--command=cabal repl pencil\""
nix-shell --attr env --run "ghcid \"--command=cabal repl pencil-example-complex\""

