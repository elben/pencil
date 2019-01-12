# Development

```bash
nix-env -iA nixpkgs.haskellPackages.doctest

nix-shell --attr env release.nix

# Inside a nix-shell:
doctest src/
```

```bash
stack build --pedantic
stack test
stack exec doctest src/
```

## Documentation

Generate documentation.

```
stack haddock --keep-going
```

## Ctags

```bash
stack install hasktags
make tags
```

## Release

Check for newer dependency versions: http://packdeps.haskellers.com/feed?needle=pencil

Make sure it builds, passes tests, and works:

```
stack build

# stack test runs tests and also the example websites (e.g.
# pencil-example-blog), generating files in their perspective `examples/out/`
# directories.
stack test

stack haddock
stack sdist
```

Check that tutorials are updated.

Update the CHANGELOG.md.

Update the version number in `pencil.cabal`.

Commit the changes.

```
git commit -m "Release version 0.x.x"
```

Tag the release:

```
git tag v0.1.0
git push --tags
```

Make sure the [Travis build is green](https://travis-ci.org/elben/pencil).

Push to Hackage:

```
stack sdist
stack upload
```

## Travis CI

[travis-ci.org/elben/pencil](https://travis-ci.org/elben/pencil)

Note that `.travis.yml` was generated using [multi-ghc-travis](https://github.com/haskell-hvr/multi-ghc-travis) this:

```
stack exec runghc ~/code/multi-ghc-travis/make_travis_yml_2.hs pencil.cabal > .travis.yml
```
