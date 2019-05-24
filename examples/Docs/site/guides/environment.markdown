# The Environment

When you load and render pages in Pencil, an environment of variables is calculated. This is what allows you to reference variables defined from other pages.

But Pencil’s variable closure works differently than most programming languages. Consider this example:

```
print value
if (true) {
  let value = "Hello!"
}
```

In most languages, this wouldn’t run because (1) you can’t use something before it’s declared and (2) `value`’s scope exists only inside the `if`-block. But because Pencil is purpose-built for building web pages, we bashfully reject these sane limitations.

**Pencil’s variable scoping** allows any page in the structure to reference any variable from any other page in the structure.

```Haskell
layout <- load "layout.html"
about <- load "about.html"

render $ layout <|| about
```

Both `layout.html` and `about.html` can declare new variables in the preamble, and they can access each other’s variables. They can also access, of course, the “global” variables defined in the `Config`’s environment.

The reason for this is simple: it’s often the case that the outer pages (e.g. the layout) will need to use a variable defined in the inner pages. When you render a blog post, the blog post’s title is defined in the inner page, but the layout may need to know it as part of its header tag: `<h1>$${postTitle}</h1>`.

The reverse is also true. Often times, an inner page will need access to an outer page’s variables. For example, a comment widget that requires the blog post ID.

**Duplicate Variable Rule**. If two pages in the structure define the same variable name, then the value is based off the context:

- If the variable is defined in the page, then the page gets that variable’s value.
- Otherwise, the deepest page’s value wins.

## Manipulating the Environment

Though page preambles and variable scoping provide a common pattern for most pages, sometimes you’ll need to manipulate the variable values yourself, in Haskell code. Pencil provides several helper functions to help you gain access to the raw variable context.

As seen in the first tutorial, you can define “global” variables in the config.

```haskell
config :: Config
config =
  (updateEnv (insertText "title" "My Awesome Website")
   defaultConfig
```

These variables follow the same rule as page-defined variables. Think of them as a part of a “Page 0” that begins every structure.

You can use `updateEnv` with methods like `insertEnv`, `insertText`, `insertPages`, `updateEnvVal`.

You can also use `getPageEnv` and `setPageEnv`, along with the functions above, to manipulate each page’s environment to your liking. These are the functions that `Pencil.Internal.Blog` uses to build up the default blogging functionality.

## Running inside a modified environment

If you modify an environment, you’ll want to run code _inside_ that environment. Pencil provides `withEnv`, which is one way to do it:

```haskell
postsEnv <- (insertPages "posts" posts env >>= insertPages "recommendedPosts" recommendedPosts)

withEnv postsEnv (render (layoutPage <|| indexPage))
```

You can also use `Control.Monad.Reader`’s `local` (which is re-exported by the Pencil module, so you don’t need to import any new modules). The example below modifies the display value function to `toTextRss`, so that the RSS content is HTML-escaped.

```haskell
local (setDisplayValue toTextRss) (render rssFeedStruct)
```