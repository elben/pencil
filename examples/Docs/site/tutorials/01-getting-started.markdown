# Tutorial 1: Getting Started

This tutorial walks you through our first Pencil website. At the end of this
tutorial, you'll understand some of the core Pencil concepts and have the
beginnings of a website built out.

You may find it useful to also have [Pencil's Haddock
page](https://hackage.haskell.org/package/pencil/docs/Pencil.html)
open as a reference.

We'll be using [stack](http://haskellstack.org) in this tutorial, so make sure
you have it installed. Let's create our project:

```sh
stack new my-website simple --resolver=lts-13.21
cd my-website
```

Open `my-website.cabal` and look for the `executable my-website-exe` section. Add `pencil` into the `build-depends` section. It should look something like this:

```
executable my-website
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , pencil
```

We'll also need to add `pencil` as as an `extra-deps` in `stack.yml`, since
`pencil` is not in Stackage yet:

```yaml
extra-deps:
  - pencil-1.0.0
```

Now we're going to add some source files. First, let's make a new directory called
`site/`, that will contain all of our website's HTML, Markdown and CSS files.

```
mkdir site
```

Our `my-website` folder should have a directory structure that looks something
like this:

```
my-website.cabal
src/
  Main.hs
site/
```

Create a new file, `site/layout.html`. This will be our website's main
template. Copy-and-paste this into `layout.html`:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>$${title}</title>
    <link rel="stylesheet" type="text/css" href="stylesheet.css"/>
  </head>
<body>
  <div class="structure">
    $${body}
  </div>
</body>
</html>
```

Notice that `layout.html` contains the strings `$${title}` and `$${body}`. These
are variables, and they allow us to dynamically inject content into this shared
layout.

Let's also create a stylesheet. Create a new file in `site/` called
`stylesheet.scss`, with this content:

```
$bgColor: #ffffff;

body {
  background-color: $bgColor;
  font-family: sans-serif;
  font-size: 18px;
}

.structure {
  margin-left: auto;
  margin-right: auto;
  width: 600px;
}
```

Notice that we're using the `.scss` extension, and we have that weird
`$bgColor` thing. This is because we're using
[Sass/Scss](http://sass-lang.com) for our styling. I like Scss because it's a
super set of CSS, so you can write plain-old CSS but "add on" the Scss parts
(like variables) when you need it.

The final source file we'll add is `index.markdown`. This will contain our index
page's content in Markdown. You'll see how easy it is convert Markdown to
HTML, and inject it into our HTML-based layout.

`index.markdown` contains:

```
# My Awesome Website

Welcome to my *awesome* [website](http://example.com)!
```

## Writing some Haskell

OK, let's write some Haskell! Fill `src/Main.hs` with this:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil

website :: PencilApp ()
website = do
  index <- load "index.markdown"
  render index

main :: IO ()
main = run website defaultConfig
```

Let's build our project and try it out.

```
stack build
stack exec my-website
```

This should create an `out/` directory with an `index.html` file, which contains
our Markdown content rendered as HTML. It's basic stuff, but we're getting
somewhere.

## Rendering Pages

Let's look at what's happening inside the `website` function:

```haskell
website :: PencilApp ()
website = do
  index <- load "index.markdown"
  render index
```

The first thing you see is `index <- load "index.markdown"`. This is the
primary way we load source files in Pencil. `load` will open the given file
and convert it if necessary. It's smart enough to know, based off the file
extension, *how* to convert the files. So Markdown becomes HTML, and SCSS
becomes CSS. This is done under the `IO` monad because it's not a pure
function (it's reading a file). This is why we save the result to `index`
using `<-` inside a `do` block.

`index` is a `Page`. A `Page` holds a page's content (e.g. HTML tags) and
variables (e.g. `$${title}` and `$${body}` as seen in `layout.html`). It's
an important data type in Pencil.

And finally we _render_ the page into an actual HTML file by calling `render index`.
Underneath the hood, `render` replaces variables in the HTML with their values.

## Structuring our Pages

Of course most websites are too complex for a single Markdown file. We want
templates and CSS styling.

Change the `website` function to this:

```haskell
website :: PencilApp ()
website = do
  layout <- load "layout.html"
  index <- load "index.markdown"
  render (layout <|| index)

  loadAndRender "stylesheet.scss"
```

The call to `loadAndRender` loads and compiles our Scss file into `stylesheet.css` in
our output directory. Look at the source code of
[`loadAndRender`](https://hackage.haskell.org/package/pencil/docs/Pencil.html#v:loadAndRender).
It's just a call to `load` with a `render` at the end.

`layout <- load "layout.html"` is familiar—we load a layout file into a `Page`.
But is `(layout <|| index)` about?

It's common to share some common template across many pages. Specifically, we want the
contents of a page to be injected into another page. In this case, we want the contents of `index.markdown` inside the `$${body}`
position of `layout.html`.

To do this, Pencil provides the concept of a `Structure`. A `Structure` is a
list of `Page`s, defining a nesting order. Think [Russian nesting
dolls](https://en.wikipedia.org/wiki/Matryoshka_doll). The first element
defines the outer-most container, and subsequent elements are _inside_ the
previous element.

<div class="note">
Underneath the hood, a `Structure` is a `NonEmpty Page`, which is a list that cannot be empty.
You can read more about `Structure`s
[here](https://hackage.haskell.org/package/pencil/docs/Pencil.html#g:3).
</div>

When you have two Pages, you can combine them into a Structure using `(<||)`
(pronounced "smash"). So `(layout <|| index)` tells Pencil to insert the
contents of `index` into the `$${body}` variable of `layout`.

There is also another method, `(<|)` (pronounced "push") that pushes a
`Page` into an exiting `Structure`.

For example, if we had a global layout and an _inner_ layout, we could do this:

```haskell
layout <- load "layout.html"
inner <- load "inner.html"
index <- load "index.markdown"

render (layout <|| inner <| index)
```

The `$${body}` variable in `inner.html` will be replaced with the contents of
`index.markdown`. And that _combined_ content is what replaces the `$${body}`
variable in `layout.html`.

Back in our original example, we need to add the `title` variable into our
`Config` for the layout's `$${title}` variable. So let's create our own
called `config`, which is a modified version of `defaultConfig`:

```haskell
config :: Config
config =
  updateEnv (insertText "title" "My Awesome Website")
            defaultConfig

main :: IO ()
main = run website config
```

Check out the
[documentation](https://hackage.haskell.org/package/pencil/docs/Pencil.html) for
more information on `updateEnv` and `insertText`.

<div class="note">
`PencilApp` is Pencil's monad transformer. Don't worry if you aren't familiar
with monad transformers.  In simple terms, `PencilApp` is a function that takes
a `Config` and does a bunch of stuff under the `IO` monad (e.g. reading source files, converting Markdown to HTML, and writing HTML files).

This is why we have to "run" our `website` function inside `main`; we have to
give the `PencilApp` function a `Config`. For now, we just pass in the default
provided by Pencil, `defaultConfig`.
</div>

# Generating our website

To generate and serve our website, run the following commands:

```
stack build
stack exec my-website
cd out && python -m SimpleHTTPServer 8000
```

Go to [http://localhost:8000](http://localhost:8000). Note that we're using
Python's HTTP server to serve our HTML files so that our relative URLs work
correctly.

And that's it! In this tutorial, you learned several important concepts:

- `load` is the primary way we load source files into `Page`s.
- A `Page` knows about our text content and template variables.
- You can smash `Page`s together into a `Structure` using `(<||)`, and reference
  them using the `${body}` template variable.
- You can set global variables in the `Config`.

Next, we'll setup continuous integration with CircleCI and GitHub Pages for
automatic deployments. Continue onward to [Tutorial 2: Deploying to GitHub Pages using Circle](/pencil/tutorials/02-deploying-to-github-pages-using-circle/)