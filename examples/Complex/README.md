# Example: Complex

This example captures complex use-cases for regression testing.

```sh
nix-shell --attr env
[nix-shell] cabal test pencil-example-complex

# In another terminal
cd examples/Complex/out
python -m SimpleHTTPServer 8000
```

Go to [localhost:8000](http://localhost:8000).
