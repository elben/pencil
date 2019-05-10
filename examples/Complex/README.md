# Example: Complex

This example captures complex use-cases for regression testing.

```
nix-shell --attr env
cabal test pencil-example-complex
cd examples/Complex/out
python -m SimpleHTTPServer 8000
```

Go to [localhost:8000](http://localhost:8000).
