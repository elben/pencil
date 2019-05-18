# Pages and Structures

Pages and Structures are fundamental data types in Pencil. Understand them and you shall succeed.

A **Page** contains:

- The contents of the file that you loaded.
- The outbound file path.
- Settings on _how_ the page should be rendered. For example, `escapeXml :: Page -> Page`  is a function that will tell the page to escape XML/HTML tags when rendered.

That seems easy enough. But if all we had was the `Page` type, Pencil wouldn’t be all that useful. It would be just a glorified Markdown renderer. To support page re-use, we need some way to *combine* or *stitch* different pages together. This is where the **Structure** type comes in.

Consider the canonical Pencil example:

```haskell
layout <- load "layout.html"
index <- load "index.markdown"
render $ layout <|| index
```

Here, we combine two pages, `layout` and `index`, into a single structure via `layout <|| index`. As described in the tutorials, `Structure` is essentially a non-empty linked list underneath the hood.

Here’s a diagram of a longer example:

```
render $ layout <|| a <| b <| c
```
	
So when `render` is called on this structure, Pencil does the following:

- Gathers all the variables from all the pages in the structure.
- Renders the contents of `index` with this combined environment.
	- This means that `index.markdown` can reference variables defined in `layout.html`, in `index.markdown`, or in the global environment specified in the `Config`.
- Inserts the rendered HTML content into the `body` variable.
- Now, it renders contents of `layout`. Note that because we now have `body` available, `layout.html` can use `${body}` in its contents, which is replaced with the contents of `index`.
- Writes a new file in the output directory, `index.html`.

So how did Pencil decide on the output file path?

**The Default File Path Rule**. By default Pencil uses the file path of last non-collection `Page` in the `Structure`.

In this case, that would be `index`’s file path. When we call `load "index.markdown"`, `load` looks at the original extension (`.markdown`) and automatically converts it to `.html`.

If we wanted a different output file name, we could use some of the provided methods like [`rename`](https://hackage.haskell.org/package/pencil/docs/Pencil.html#v:rename) and [`move`](https://hackage.haskell.org/package/pencil/docs/Pencil.html#v:move):

```
render $ layout <|| rename "another-name.html" index
```

## Collections

```haskell
layout <- load "layout.html"
index <- load "index.html"

post1 <- load "post1.markdown"
post2 <- load "post2.markdown"
let posts = [post1, post2]

render $ layout <|| index <<| coll "posts" posts
```


## Useful Functions

Here are some commonly-used functions. Check out the Hackage links for detailed information and examples.

### Page

- `load`
- `loadDir`
- `loadAndRender`
- `rename`
- `move`

### Structure

- `struct`
- `(<||)`
- `(<|)`