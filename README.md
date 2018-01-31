# Pencil

Pencil is a static site generator. Use it to generate your personal website!
Pencil comes pre-loaded with goodies such as blogging, tagging, templating,
and Markdown Sass/Scss support. Flexible enough to extend for your own needs.

> The blue-backed notebooks, the two pencils and the pencil sharpener... the
> marble topped tables, the smell of early morning... and luck were all you
> needed. — Ernest Hemingway, A Moveable Feast

# Examples

Checkout the [examples provided](https://github.com/elben/pencil/blob/master/test/Example/). To run the [Simple](https://github.com/elben/pencil/blob/master/test/Example/Simple) example:

```
stack build
stack exec pencil-example-simple
```

Open the `examples/Simple/out/` folder to see the rendered web pages.

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
