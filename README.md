[![CircleCI](https://circleci.com/gh/elben/pencil/tree/master.svg?style=svg)](https://circleci.com/gh/elben/pencil/tree/master)

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

Here's an example that shows a personal website with a blog and an RSS feed.
Based off the [this
example](https://github.com/elben/pencil/tree/master/examples/Simple).

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil

config :: Config
config =
  updateEnv (insertText "title" "My Awesome Website") defaultConfig

website :: PencilApp ()
website = do
  layout <- load "layout.html"
  index <- load "index.markdown"
  render (layout <|| index)

  loadAndRender "stylesheet.scss"

main :: IO ()
main = run website config
```

You can check out other [examples](https://github.com/elben/pencil/tree/master/examples). The [Blog](https://github.com/elben/pencil/tree/master/examples/Blog) is a good one.

My personal website (http://elbenshira.com) uses Pencil ([source here](https://github.com/elben/elben.github.io)). And so does Pencil's website at (http://elbenshira.com/pencil) ([source here](https://github.com/elben/pencil/tree/master/examples/Docs)).

# Development

See [DEVELOPMENT.md](DEVELOPMENT.md)