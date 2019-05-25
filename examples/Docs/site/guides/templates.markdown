# Templates

Pencil comes with a simple templating engine. Templates allow us to build web pages dynamically using Haskell code. Every blog post, for example, can share a common HTML template.

Pencil templates are regular HTML and Markdown (and other formats that Pencil supports) files that contain a **preamble** or **directives**.

## Preamble

Preambles are environment variable declarations inside your source files. A file may only have one preamble, and they must be declared at the top of the file. A preamble is YAML wrapped in an HTML comment. An example Markdown file with a preamble:

```
<!--PREAMBLE
postTitle: "Why I Love Python"
date: 2010-01-30
tags:
  - python
-->

I learned Java first. So when I found out about Python,
it was *love at first sight*.
```

In the above example, Pencil will intelligently parse the `date` value as a [`VDateTime`](https://hackage.haskell.org/package/pencil/docs/Pencil-Internal-Env.html#v:VDateTime). See [`toDateTime`](https://hackage.haskell.org/package/pencil/docs/Pencil-Internal-Env.html#v:toDateTime), which describes the date formats that Pencil will try to coerce into a `VDateTime`.

## Directives

Directives allow you to render variables. They are surrounded by `$${...}`.

### Variables

The simplest directive is the variable directive.

```
Hello $${name}!
```

The above template will render the value of the variable `name`, which is expected to be in the environment at `render`. If the variable is not found, the final render will literally include `${name}`.

### If block

The `if` directive allows us to render content based off the existence of a variable in the current environment.

```
$${if(name)}
  Hello $${name}!
$${end}
```

In this case, we now make sure that `name` is available before rendering.

### For loop

The `for` directive allows for looping over arrays. This is useful for things like rendering a list of blog post titles, linking each line to the actual blog post.

```
<ul>
$${for(posts)}
  <li><a href="$${this.url}">$${postTitle}</a> - $${date}</li>
$${end}
</ul>
```

`posts` must either be a collection node in the Structure, or a [`VEnvList`](https://hackage.haskell.org/package/pencil/docs/Pencil-Internal-Env.html#v:VEnvList) variable. The example will render each post's title, publish date, and will link it to `this.url`. Note that inside the `for` block, the scope changes to each post's environment. So `${postTitle}` will render each post's title.

`this.url` is a special variable that is automatically inserted for you inside a loaded `Page`. It points to the page's destination file path.

### Partials

The `partial` directive injects another template file into the current file. The directives inside the partial are rendered in the same environmental context as the `partial` directive.

Think of partials as just copy-and-pasting snippet from one file to another. Unlike Structures, partials cannot define environment variables.

In the example below, the first `partial` is rendered with the current
environment. The `partial` inside the `for` loop receives the same
environment as any other snippet inside the loop, and thus has access to the environment inside each post.

```
${partial("partials/nav-bar.html")}

${for(posts)}
  ${partial("partials/post-item.html")}
${end}
```

### Escaping

If you want a literal “$${name}” to be rendered, you need to *escape* the dollar sign by doubling it. So in the example below, Pencil will not parse it as a directive, but will render “$${name}” instead:

```
$$${name}
```

[This very source file](https://github.com/elben/pencil/blob/master/examples/Docs/site/guides/templates.markdown) uses escapes.