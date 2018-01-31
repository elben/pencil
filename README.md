# Pencil

Pencil is a static site generator. Use it to generate your personal website!
Pencil comes pre-loaded with goodies such as blogging, tagging, templating,
and Markdown Sass/Scss support. Flexible enough to extend for your own needs.

Start building with Pencil with these [tutorials](http://elbenshira.com/pencil)
along with the [documentation](https://hackage.haskell.org/package/pencil).

Also checkout the [examples provided](https://github.com/elben/pencil/blob/master/test/Example/). To run the [Simple](https://github.com/elben/pencil/blob/master/test/Example/Simple) example:

```
stack build
stack exec pencil-example-simple
```

Open the `test/Example/Simple/out/` folder to see the rendered web pages.

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
