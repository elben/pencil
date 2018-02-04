# Development

```bash
stack build --pedantic
stack test
stack exec doctest src/
```

## Documentation

```
stack haddock
```

## Ctags

```bash
stack install hasktags
hasktags --ignore-close-implementation --ctags .
```

## Release

Make sure it builds, passes tests, and works:

```
stack build
stack test
stack exec pencil-example-simple
stack exec pencil-example-blog
stack haddock
```

Check that tutorials are updated.

Update the CHANGELOG.md.

Update the version number in `pencil.cabal`.

Commit the changes.

Tag the release:

```
git tag v0.1.0
git push --tags
```

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
