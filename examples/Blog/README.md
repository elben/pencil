# Example: Blog

This example contains a full blogging system. Your blog posts can have tags.
This example also renders tag index pages.

```sh
nix-shell --attr env
[nix-shell] cabal test pencil-example-blog

# In another terminal
cd examples/Blog/out
python -m SimpleHTTPServer 8000
```

Go to [localhost:8000](http://localhost:8000).
