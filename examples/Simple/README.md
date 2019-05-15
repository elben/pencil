# Example: Simple

This is a simple example of a basic website.

```sh
nix-shell --attr env
[nix-shell] cabal test pencil-example-simple

# In another terminal
cd examples/Simple/out
python -m SimpleHTTPServer 8000
```

Go to [localhost:8000](http://localhost:8000).