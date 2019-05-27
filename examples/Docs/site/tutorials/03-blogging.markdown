# Tutorial 3: Blogging

This tutorial walks through how we can set up a blog on our website by following the [Blog example](https://github.com/elben/pencil/tree/master/examples/Blog) found in the Pencil `examples/` repo.

Let's consider all the features we'd want for our blog:

- Each blog post should get its own URL. Something like `mywebsite.com/blog/my-first-post/`.
- Be able to add tags to a post.
- An archive page that lists all the blogs post. Perhaps at `mywebsite.com/blog`.
- An archive page per tag, listing all posts tagged with that tag.
- An RSS feed.

That's a pretty thorough feature list. But because Pencil aims to make life as easy for you as possible, it's loaded with a bunch of these things. They're not "raw" functionality built into Pencil, but more like user-land features that were so useful that it made it into the core library. And it's all included when you `import Pencil`.

Pencil also ascribes to the philosophy of convention over configuration. And though you could configure these various tools to your liking or even write your own version (none of the functions are too complicated or very long), will follow Pencil's convention for blogging.

So let's get started!

## Rendering blog posts

Unlike the first tutorial, we're not going to walk through each file you need to create and so forth. Since we're following the [Blog example](https://github.com/elben/pencil/tree/master/examples/Blog), I trust that you'll be able to follow along in your own code base.

First, peruse the `site/` folder ([here](https://github.com/elben/pencil/tree/master/examples/Blog/site)) and get comfortable with a couple of things in there.

- The `blog/` directory contains all of our blog posts, in Markdown format.
- [`layout.html`](https://github.com/elben/pencil/blob/master/examples/Blog/site/layout.html) is the layout we'll use for every page we render.
- [`post-layout.html`](https://github.com/elben/pencil/blob/master/examples/Blog/site/post-layout.html) is the layout we'll use for each actual blog post. Notice the for-loop in there for each tag.

If you open `2018-01-30-code-related-stuff.markdown`, you'll see a preamble section at the beginning.

```html
<!--PREAMBLE
date: 2018-01-30
postTitle: "Code related stuff"
tags:
  - awesome
-->
```

Note that `date` and `postTitle` are required. We also see that this one was tagged with "awesome". We'll get to tags further down.

Now, for our first stab:

```Haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

websiteTitle :: T.Text
websiteTitle = "My Blog"

config :: Config
config =
  (updateEnv (insertText "title" websiteTitle) .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/"
  ) defaultConfig

website :: PencilApp ()
website = do
  loadAndRender "assets/style.scss"
  
  layout <- load "layout.html"
  postLayout <- load "post-layout.html"

  posts <- loadPosts "blog/"
  
  render $ fmap ((layout <|| postLayout <|)) posts
  
main :: IO ()
main = run website config
```

With these first few lines of code, you should be able to see something working. As usual:

```
stack exec my-website
cd out && python -m SimpleHTTPServer 8000
```

And you should see our blog posts rendered! For example: [http://localhost:8000/blog/code-related-stuff/](http://localhost:8000/blog/code-related-stuff/). Of course it's not listing any tags yet, and we don't have a blog index or anything. But it's a start.

Now, you should be familiar with the `load` function by now. But here we see `loadPosts`. This is a helper function provided by Pencil to do the following:

- Load all pages found in the `blog/` folder.
- Rename the file path from `blog/2018-01-31-ramblings.markdown` to `blog/ramblings/`.
- Sort by the date specified in each page's `date` variable found in the preamble.
- And return the list of `Page`s.

Don't be shy about reading the source code. You'll find that I sent more characters explaining it above than the actual source of `loadPosts`.

To render each post, we had this:

```haskell
render $ fmap ((layout <|| postLayout <|)) posts
```

This converts a list of `Page`s into a list of `Structure`s, pushing each page into the layout structure. We then render the list of `Structure`.

<div class="note">
There's an instance of the `Render` typeclass for list of `Page`s. That's why you can call `render` on a list.
</div>

If you loaded one of the rendered pages in your browser, you'll notice that the `<title>` tag still just says "My Blog". But shouldn't it contain the title of the blog post also?

Well, this ain't the first time someone ran into this problem. Use `injectTitle` to fix it:

```
render $
  fmap ((layout <|| postLayout <|) . injectTitle websiteTitle)
    posts
```

`injectTitle` takes the `postTitle` variable in each post's preamble and puts it in the front of the given website title. Rebuild the website, and the "Code related stuff" post now has the title "Code related stuff - My Blog".

<div class="note">
The dot operator `(.)` is function composition. `(a . b . c) d` translates to `a (b (c d))`.
</div>

## Listing all blog posts

We'll of course want to build a page that lists out all of our blog posts. To do that, let's add a couple of lines to our `website` function:

```
index <- load "index.html"
render (layout <|| index <<| coll "posts" posts)
```

Ooo, some new stuff! Let's talk about structures. As you know from the previous tutorials (and reference pages, and the [Pencil docs](http://hackage.haskell.org/package/pencil/docs/Pencil.html)) A structure is a nesting of pages. But sometimes we'll want to nest not just a single page, but a collection of pages. In our case, we need our index page to have access to the list of post pages so that we can, in our template, loop through each post and display its `postTitle` along with its `date` and URL.

Well, that's possible, as long as the collection element is (1) always the last element in the structure and (2) not the first. So you can't have a structure with just a collection, for example.

This rule is there to keep things simple, and it makes sense too. For most use-cases, the collection is the "most important thing" for the page we're trying to build.

Let's talk about the contents of `index.html` too.

```html
<h1>My Blog</h1>

<p>This is my blog. Check out my blog posts:</p>

${partial("post-list.html")}

<p><a href="/rss.xml">RSS Feed</a></p>
```

First, ignore the RSS stuff—we'll get to it later. But look out that `${partial()}` template directive. A partial is just a copy-paste of the contents in the partial. Nothing fancy is going on. So we'll need to check out `post-list.html`:

```html
<ul>
  ${for(posts)}
    ${partial("post-bullet.html")}
  ${end}
</ul>
```

Interesting! Here we loop over the `posts` variable. This comes from that collection structure that we built, when we did `coll "posts" posts`. For each post, we render the `post-bullet.html` partial:

```html
<li>
  <a href="${this.url}">${postTitle} - ${date}</a>
</li>
```

What you'll want to understand here is the fact that when we are inside the for-loop, the _environment context_ has changed. We have access to the post's variables like `postTitle` and `date`. And note that `this.url` is _automatically_ set by Pencil during render.

So if we build our website now, we'll see a sweet index page listing all of our blog posts!

**Exercise**: Create a new blog post and re-build your website. Reference variables in your blog post, like `${postTitle}` and `${title}` (which is the website's main title).

## Tags, Tags, Tags

So we are now rendering each blog post, and we have a listing of blog posts. Time to deal with tags.

A bit of warning: there's some hand-wavy stuff in this section. Not because it's magical, but because it can be confusing to explain without actually looking at the implementation.

Add these lines to the `website` function:

```haskell
tagPages <- buildTagPages "tag-list.html" posts
render $ fmap (layout <||) (H.elems tagPages)
```

`buildTagPages` takes `tag-list.html` and a list of post pages. It returns (hand-waving about to happen) a hash map of tags (which is just a `Text`) to a `Page`. That `Page` is a version of the page loaded from `tag-list.html`, but with the `tag` variable set to the tag in the key, and a `posts` array containing all posts that was tagged with the current tag. Oh, and the page's URL is set to `blog/tags/${tag}/`.

Yeah, `buildTagPages` is complicated. If you want to control some of the configuration, use `buildTagPagesWith`.

So as an example, the value for the key `"awesome"` is a `Page` where the `posts` variable contain `Page`s containing the posts from `2018-01-30-code-related-stuff.markdown` and `2018-02-01-more-ramblings.markdown`, since they were both tagged with `awesome`. The URL for the page is `blog/tags/awesome/`.

We actually can now page for each tag, listing out all the posts tagged with the tag. This is what `render $ fmap (layout <||) (H.elems tagPages)` does. It takes each `Page`, which is a version of `tag-list.html`, and renders it.

Re-build your website, and check out [localhost:8000/blog/tags/awesome/](http://localhost:8000/blog/tags/awesome/).

OK that's pretty sweet. But we also want to list out the tags on each of the blog post page that we rendered. And not only that, but we want to link each tag to the tag listing page. This is common-enough and confusing-enough to warrant a built-in method. We can use `injectTags` during the blog post render, like this:

```
render $
  fmap ((layout <|| postLayout <|)
        . injectTags tagPages
        . injectTitle websiteTitle)
    posts
```

Basically we're giving each post the entire mapping of tags to posts. Open `post-layout.html` and notice this:

```
${if(tags)}
  Tags: ${for(tags)} <a href="${this.url}">${tag}</a> ${end}
${end}
```

`injectTags` filters out the `tagPages` you passed in only to the tags found in the current page. Thus we can loop through `tags` and link to the tag listing page, just like that.

There ya go. Tags. It's complicated underneath the hood, but at least the method are easy to use, right? As always, if you're interested in learning more, the source code is always available. If you want a more interesting tagging system (sub-tags?), the existing implementation would serve as a good starting point.

## RSS

The final thing we'll tackle is RSS. Let's look at `rss.xml`:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<rss version="2.0" xmlns:atom="http://www.w3.org/2005/Atom">
<channel>
  <title>Example RSS Feed</title>
  <link>http://example.com/rss.xml</link>
  <description>An example RSS feed.</description>

  ${for(posts)}
  <item>
  <link>https://example.com${this.url}</link>
  <guid>https://example.com${this.url}</guid>
  <pubDate>${date}</pubDate>
  <title>${postTitle}</title>
  <description>${this.content}</description>
  </item>
  ${end}
</channel>
</rss>
```

This is our template. We loop over ever post and render the post's URL, title, date and content. `this.content` is something we've never seen before. Any page's content is always available through the special `this.content` variable. So to render each page's content, all we need to do is type `${this.content}`.

We use `this.content` instead of `body` because `body` refers to the *inner* page's content. But inside the for-loop, the context is the current page being looped over. And our post page does not have an inner page to render, so `body` is not available.

Modifying `website`, we can add:

```Haskell
rssLayout <- load "rss.xml"

let rssFeedStruct = struct rssLayout <<|
  coll "posts" (fmap escapeXml (take 10 posts))

local (setDisplayValue toTextRss)
    (render rssFeedStruct)
```

I think we understand `load "rss.xml"` by now, but what is that monstrosity in the next line? Examining it from right-to-left:

- Take only the ten newest posts.
- Mark each `Page` as `escapeXml`. This is because we need the HTML tags of our posts to be escaped when we render it in the `<description>` tag. The `escapeXml` function just marks a boolean inside of the `Page` to note it to escape tags whenever the page is rendered.
- We've seen `coll "posts"` before. It puts our ten pages into the structure as a collection.
- `struct page` initializes the page in a structure. Then we push the collection element into the structure using `<<|`.

So now we have a structure containing all of the RSS posts we want to render. Can't we call `render rssFeedStruct`? Well, almost.

The RSS specification expects the `<pubDate>` value to be in the RFC 822 date format (e.g. `Wed, 31 Jan 2018 00:00:00 +0000`). But Pencil's default renderer will render a `VDateTime` in `YYYY-MM-DD` format. The transformation of `Value`s to their final string outputs is defined in the config by [`configDisplayValue`](http://hackage.haskell.org/package/pencil/docs/Pencil-Internal-Pencil.html#v:configDisplayValue). As you can see in the Hackage documentation, the default value is [`toText`](http://hackage.haskell.org/package/pencil/docs/Pencil-Internal-Env.html#v:toText). We need to replace `toText` with a different one, a Pencil-supplied one called [`toTextRss`](http://hackage.haskell.org/package/pencil/docs/src/Pencil.Env.Internal.html#toTextRss). The only difference between the two is how `VDateTime` values are rendered.

We could update our `config` value to:

```haskell
config :: Config
config =
  (updateEnv (insertText "title" websiteTitle) .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/" .
   setDisplayValue toTextRss
  ) defaultConfig
```

But that would change how dates are rendered *everywhere*. What we need to do is *temporarily* set `configDisplayValue` too `toTextRss`.

This is where [`local`](https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Reader.html#v:local) comes in. `local` allows us to temporarily run some command in a *modified* `Config` environment.

```
local (setDisplayValue toTextRss) (render rssFeedStruct)
```

The above means that `render rssFeedStructure` runs with `configDisplayValue` set to `toTextRss`.

Let's re-compile our website one more time and check out [localhost:8000/rss.xml](http://localhost:8000/rss.xml). We have an RSS feed indeed.

## Conclusion

In this tutorial you learned:

- How to use Pencil's built-in blogging functionality to load blog posts, render them, build an archive page, and deal with tags.
- How partials work.
- How to push collection of pages into a structure.
- How to run commands in a temporarily-modified `Config` environment.