module Pencil
  (
    -- * Getting started
    --
    -- $gettingstarted

    -- * Templates
    --
    -- $templates

  -- * Pages, Structures and Resources
  --
  -- $pagesStructuresResources

    Page
  , getPageEnv, setPageEnv
  , load
  , load'
  , loadDir
  , loadDir'
  , withEnv
  , loadAndRender
  , useFilePath
  , escapeXml
  , rename
  , move

  , Structure
  , struct
  , (<||)
  , (<|)
  , (<<|)
  , coll

  , Resource
  , passthrough
  , loadResource
  , loadResources
  , listDir

  , Render(..)
  , toExpected
  , toHtml
  , toCss
  , toDir

  -- * Blogging
  --
  -- $blogging

  , loadBlogPosts
  , blogPostUrl
  , injectTitle
  , Tag
  , buildTagPages
  , buildTagPagesWith
  , injectTagsEnv

  -- * Environment Manipulation

  , merge
  , insertEnv
  , insertText
  , insertPages
  , updateEnvVal
  , sortByVar
  , filterByVar
  , groupByElements
  , toTextRss

  -- * Configuration

  , Config
  , defaultConfig
  , getSourceDir, setSourceDir
  , getOutputDir, setOutputDir
  , getEnv, setEnv, updateEnv
  , getDisplayValue, setDisplayValue
  , getSassOptions, setSassOptions
  , getPandocReaderOptions, setPandocReaderOptions
  , getPandocWriterOptions, setPandocWriterOptions

  -- * Utils and Re-exports

  , FileType
  , fileType
  , toExtension

  -- Re-exports
  , Reader.asks

  -- * Running and Error Handling

  , PencilApp
  , PencilException
  , run

  ) where

import Pencil.Internal.Pencil
import Pencil.Internal.Blog

import Control.Monad.Reader as Reader

----------------------------------------------------------------------

-- $gettingstarted
--
-- The best way to get started is to follow the tutorials found on
-- [elbenshira.com/pencil](https://elbenshira.com/pencil).
--
-- [Here](https://github.com/elben/pencil/blob/master/examples/Simple/src/Main.hs)
-- is an example Pencil app, which is a very simple website with only a couple
-- of pages. @Main.hs@ looks like this:
--
--
-- > module Main where
-- > 
-- > import Pencil
-- > 
-- > config :: Config
-- > config =
-- >   (updateEnv (insertText "title" "My Awesome Website") .
-- >    setSourceDir "examples/Simple/site/" .
-- >    setOutputDir "examples/Simple/out/")
-- >    defaultConfig
-- > 
-- > website :: PencilApp ()
-- > website = do
-- >   layout <- load "layout.html"
-- >   index <- load "index.markdown"
-- >   render (layout <|| index)
-- > 
-- >   loadAndRender "stylesheet.scss"
-- > 
-- > main :: IO ()
-- > main = run website config
--
--
-- First, we need to set up a 'Config'. We start with 'defaultConfig', and
-- modify it to specify where the source and output files go. We also add a
-- @title@ variable with the value @"My Awesome Website"@ into the environment.
--
-- An 'Pencil.Internal.Env.Env', or environment, is a mapping of variables to
-- its values. A variable can hold a string, number, boolean, date, and so on.
-- We can use variables in our web pages via a variable directive like
-- @${title}@.
--
-- Let's look at the @website@ function. Note that its type is @PencilApp ()@.
-- 'PencilApp' is the monad transformer that web pages are built under. Don't
-- worry if you aren't familiar with monad transformers; in simple terms,
-- @PencilApp@ is a function that takes a @Config@, and does all the source file
-- loading and web page rendering under the @IO@ monad. So @website@ is a
-- function that is waiting for a @Config@. So to actually run our app, we need
-- to "give" @website@ a @Config@:
--
-- @
-- run website config
-- @
--
-- Now let's dissect the @website@ function itself. The first thing we do is
-- @'load' "layout.html"@, which loads our layout file into something called a
-- 'Page'. In short, a 'Page' holds the contents of the file, plus the
-- environment of that file, plus the destination file path.
--
-- 'load' looks at the file extension to figure out that @layout.html@ is an
-- HTML file, and @index.markdown@ is a Markdown file. Both files will
-- eventually be rendered with a @.html@ extension.
--
-- Now, what about @render (layout <|| index)@. What the heck is going on here?
-- In plain language, you can think of @(layout <|| index)@ as "pushing" the
-- contents of @index@ into @layout@. When @index@ is rendered, the
-- Markdown is converted to HTML, and variables are replaced with their values.
-- The final rendered content is then stuffed into a special @body@ variable in
-- @layout@'s environment. This means that when @layout@ is rendered, the
-- @${body}@ in @layout@ is replaced with the contents of @index@.
--
-- @(layout <|| index)@ describes what /will/ happen. @(<||)@ combines the two
-- pages together into something called a 'Structure'. Passing this structure
-- into 'render' is what actually generates the web page.
--
-- Finally, we have @'loadAndRender' "style.scss"@, which is a helper method to load
-- and render files in one step, automatically converting and renaming files.
--
-- And that's it! If you run this code, it will spit out an @index.html@ file
-- and a @style.css@ file in the @examples\/Simple\/out\/@ folder.
--
-- To learn more, dig into the tutorials and guides found on
-- [elbenshira.com/pencil](elbenshira.com/pencil).

----------------------------------------------------------------------

-- $templates
--
-- Pencil comes with a simple templating engine. Templates allow us to build web
-- pages dynamically using Haskell code. Your blog posts, for example, can share
-- a single HTML template.
--
-- Pencil templates are regular text files that can contain a /preamble/ and
-- /directives/.
--
-- == Preamble
--
-- Preambles are environment variable declarations inside your source files.
-- They should be declared at the top of the file, and a file may only have one
-- preamble. They're YAML wrapped in an HTML Comment. Example preamble, in the
-- first part of @my-blog-post.markdown@:
--
-- > <!--PREAMBLE
-- > postTitle: "Behind Python's unittest.main()"
-- > date: 2010-01-30
-- > tags:
-- >   - python
-- > -->
--
-- In the above example, Pencil will intelligently parse the @date@ value as a
-- `Pencil.Internal.Env.VDateTime`.
--
-- == Directives
--
-- Directives are rendering /commands/. They are surrounded by @${...}@.
--
-- === Variables
--
-- The simplest directive is the variable directive.
--
-- @
-- Hello ${name}!
-- @
--
-- The above template will render the value of the variable @name@, which is
-- expected to be in the environment at 'render'. If the variable is not found,
-- the final render will include the directive (e.g. "@${name}@").
--
-- === If block
--
-- The @if@ directive allows us to render content based off the existence of a
-- variable in the current environment.
--
-- > ${if(name)}
-- >   Hello ${name}!
-- > ${end}
--
-- In this case, we now make sure that @name@ is available before rendering.
--
-- === For loop
--
-- The @for@ directive allows us to loop over arrays. This is useful for things
-- like rendering a list of blog post titles, linking each line to the actual
-- blog post.
--
-- > <ul>
-- > ${for(posts)}
-- >   <li><a href="${this.url}">${postTitle}</a> - ${date}</li>
-- > ${end}
-- > </ul>
--
-- Assuming that @posts@ was added as a collection in the Structure, this will
-- render each post's title, publish date, and will link it to @this.url@. Note
-- that inside the @for@ block, the scope changes to each post's environment.
-- So @${postTitle}@ will render each post's title.
--
-- @this.url@ is a special variable that is automatically inserted for you
-- inside a loaded @Page@. It points to the page's destination file path.
--
-- === Partials
--
-- The @partial@ directive injects another template file into the current file.
-- The directives inside the partial are rendered in the same environmental
-- context as the @partial@ directive.
--
-- Think of partials as just copy-and-pasting snippet from one file to another.
-- Unlike 'Structure's, partials cannot define environment variables.
--
-- In the example below, the first @partial@ is rendered with the current
-- environment. The @partial@ inside the @for@ loop receives the same
-- environemnt as any other snippet inside the loop, and thus has access to
-- the environment inside each post.
--
-- > ${partial("partials/nav-bar.html")}
-- >
-- > ${for(posts)}
-- >   ${partial("partials/post-item.html")}
-- > ${end}

----------------------------------------------------------------------

-- $pagesStructuresResources
--
-- 'Page', 'Structure' and 'Resource' are the "big three" data types you need to
-- know to effectively use Pencil.

----------------------------------------------------------------------

-- $blogging
--
-- This module provides a standard way of building and generating blog posts.
-- Check out the Blog example
-- <https://github.com/elben/pencil/blob/master/examples/Blog/ here>.
--
-- To generate a blog for your website, first create a @blog/@ directory in
-- your web page source directory.
--
-- Then, name your blog posts in this format:
--
-- > yyyy-mm-dd-title-of-blog-post.markdown
--
-- Where @yyyy-mm-dd@ should be something like @2019-12-30@. This isn't used for
-- anything other than to keep each post ordered in the directory, for your ease
-- of viewing.
--
-- Each post is expected to have a preamble that has at least @postTitle@ and
-- @date@ defined. The date set in the preamble is used as the sort order of the
-- blog posts. The other variables are optional.
--
-- > <!--PREAMBLE
-- > postTitle: "The Meaning of Life"
-- > date: 2010-01-30
-- > draft: true
-- > tags:
-- >   - philosophy
-- > -->
--
-- You can mark a post as a draft via the @draft@ variable, so that it won't be
-- loaded when you call 'loadBlogPosts'. You can also set the post's tags using,
-- as seen above in @tags@. Then, use 'loadBlogPosts' to load the entire @blog/@
-- directory.
--
-- In the example below, @layout.html@ defines the outer HTML structure (with
-- global components like navigation), and @blog-post.html@ is a generic blog
-- post container that renders @${postTitle}@ as a header, @${date}@, and
-- @${body}@ for the post body.
--
-- @
-- layout <- 'load' "layout.html"
-- postLayout <- 'load' "blog-post.html"
-- posts <- 'loadBlogPosts' "blog/"
-- render (fmap (layout <|| postLayout <|) posts)
-- @

----------------------------------------------------------------------