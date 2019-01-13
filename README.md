[![Build Status](https://travis-ci.org/elben/pencil.svg?branch=master)](https://travis-ci.org/elben/pencil)

# Pencil

Pencil is a static site generator. Use it to generate your personal website!
Pencil comes pre-loaded with goodies such as blogging, tagging, templating,
and Markdown Sass/Scss support. Flexible enough to extend for your own needs.

The easiest way to get started is to read the tutorials at
[elbenshira.com/pencil](http://elbenshira.com/pencil) and reference the [Haddock
docs](https://hackage.haskell.org/package/pencil).

> The blue-backed notebooks, the two pencils and the pencil sharpener... the
> marble topped tables, the smell of early morning... and luck were all you
> needed. — Ernest Hemingway, A Moveable Feast

# Examples

Checkout the [examples provided](https://github.com/elben/pencil/tree/master/examples). To run the [Simple](https://github.com/elben/pencil/tree/master/examples/Simple) example:

```
curl https://nixos.org/nix/install | sh

# TODO fix

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

See [DEVELOPMENT.md](DEVELOPMENT.md)
