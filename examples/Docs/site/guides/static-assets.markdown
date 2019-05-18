# Dealing with Static Assets

Some files don't need any special manipulation. We just want to either copy-and-paste the files from our source directory to the output directory, or a straight-forward conversion (e.g. SCSS to CSS) without needing pages and structures and environment variables.

Pencil understands this, and supplies a couple of functions to make this easy. The [Hackage documentation](http://hackage.haskell.org/package/pencil/docs/Pencil.html#t:Resource) has lots of examples. You can also look at the [Complex example](https://github.com/elben/pencil/tree/master/examples/Complex), which tests all of these scenarios.

If you have a folder of mixed assets like SCSS files and images, you can use [`loadAndRender`](http://hackage.haskell.org/package/pencil/docs/Pencil.html#v:loadAndRender):

```haskell
loadAndRender "assets/"
loadAndRender "profile-pic.jpg"
```

Convertible files will be converted, but binary files are copy-and-pasted through.

If you have files that you want to render *without* any conversion, like a Markdown file that you want to *keep as* Markdown, use [`passthrough`](http://hackage.haskell.org/package/pencil/docs/Pencil.html#v:loadAndRender):

```haskell
passthrough "assets/" >>= render
passthroguh "example.markdown" >>= render
```

You can also combine these functions with file-path-changing functions like `move` and `rename`:

```haskell
move "assets/example.markdown" <$> passthrough "assets/ex.markdown" >>= render
```

We use the functor `<$>` function (i.e. `fmap`) because `passthrough` has the type `FilePath -> PencilApp Resource`.

