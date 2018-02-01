[![Build Status](https://travis-ci.org/elben/pencil.svg?branch=master)](https://travis-ci.org/elben/pencil)

# Pencil

Pencil is a static site generator. Use it to generate your personal website!
Pencil comes pre-loaded with goodies such as blogging, tagging, templating,
and Markdown Sass/Scss support. Flexible enough to extend for your own needs.

> The blue-backed notebooks, the two pencils and the pencil sharpener... the
> marble topped tables, the smell of early morning... and luck were all you
> needed. — Ernest Hemingway, A Moveable Feast

# Examples

Checkout the [examples provided](https://github.com/elben/pencil/tree/master/examples). To run the [Simple](https://github.com/elben/pencil/tree/master/examples/Simple) example:

```
stack build
stack exec pencil-example-simple
```

Open the `examples/Simple/out/` folder to see the rendered web pages. To serve
the web pages (so that relative URLs work), using `python`'s built in web server
is easiest:

```
cd examples/Simple/out/
python -m SimpleHTTPServer 8000
```

And go to [localhost:8000](http://localhost:8000).

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
