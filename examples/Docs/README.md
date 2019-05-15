# Pencil Docs

This is the website for building the Pencil documentation website, found at [elbenshira.com/pencil](http://elbenshira.com/pencil)

```sh
nix-shell --attr env
[nix-shell] cabal test pencil-docs

# In another terminal
cd docs/
python -m SimpleHTTPServer 8000
```

Go to [localhost:8000](http://localhost:8000). Note that things won't look right
and links won't work, because the links are rooted at /pencil for it to work
when published to elbenshira.com/pencil.
