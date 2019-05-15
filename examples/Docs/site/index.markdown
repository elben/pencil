Pencil is a static site generator written in Haskell.

With Pencil, you write Haskell code to generate your website from source files
(HTML, CSS, Markdown). Pencil comes pre-loaded with blogging, tagging,
templating, and Markdown and Sass/Scss. And it's flexible enough to extend for
your own needs.

Jump right in with the first tutorial, [Getting
Started](/pencil/tutorials/01-getting-started/).

This website itself is generated via Pencil. You can view the source code
[here](https://github.com/elben/elben.github.io).

### Why use Pencil when there's already Hakyll?

I used [Hakyll](https://jaspervdj.be/hakyll/) for a while but found it a bit too
complicated for my needs. As a Haskell beginner I found it difficult extend
beyond what was provided because I couldn't understand what was going on
underneath the hood.

Pencil is a simpler, beginner-friendly alternative to Hakyll. It's especially
designed to be easy to understand and modify.

Pencil and the tutorials present here were designed with the beginner Haskeller
in mind. It is expected, however, that you have a good grasp of basic Haskell
syntax, functions, algebraic data types (the `data` keyword), records, maps and
filters, and data structures like `Data.Map`. A preliminary knowledge of how to
use monads is also expected, though you don't need to know exactly how they're
implemented.
