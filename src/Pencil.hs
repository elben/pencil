module Pencil
  (
    -- * Getting started
    --
    -- $gettingstarted

    -- * Templates
    --
    -- $templates

    PencilApp
  , run

  -- * Pages, Structures and Resources
  --
  -- $pagesStructuresResources

  , Page
  , getPageEnv, setPageEnv
  , load
  , withEnv
  , renderCss
  , useFilePath
  , escapeXml

  , Structure
  , (<||)
  , (<|)
  , (<<|)
  , structure
  , coll

  , Resource
  , loadResource
  , loadResources
  , passthrough
  , listDir

  , Render(..)
  , toHtml
  , toDir
  , toCss
  , toExpected

  -- * Environment Manipulation

  , merge
  , insertEnv
  , insertText
  , insertPages
  , insertPagesEscape
  , updateEnvVal
  , sortByVar
  , filterByVar
  , groupByElements

  -- * Configuration

  , Config
  , defaultConfig
  , getSourceDir, setSourceDir
  , getOutputDir, setOutputDir
  , getRootUrl, setRootUrl
  , getEnv, setEnv, updateEnv
  , getDisplayValue, setDisplayValue
  , getSassOptions, setSassOptions
  , getPandocReaderOptions, setPandocReaderOptions
  , getPandocWriterOptions, setPandocWriterOptions

  -- * Utils and re-exports

  , FileType
  , fileType
  , toExtension

  -- Re-exports
  , Reader.asks

  -- * Error handling

  , PencilException

  ) where

import Pencil.Internal.Pencil

import Control.Monad.Reader as Reader

----------------------------------------------------------------------

-- $gettingstarted
--
-- To get started, let's look at
-- <https://github.com/elben/pencil/blob/master/examples/Simple/Main.hs this>
-- example, which is a very simple website with only a couple of pages. Browse
-- through the
-- <https://github.com/elben/pencil/tree/master/examples/Simple/site/ site>
-- folder to see the source web pages we'll be using. You can run this example
-- by following the instructions found in the
-- <https://github.com/elben/pencil/blob/master/README.md#examples README.md>.
--
-- First, we have @layout.html@, which will serve as the layout of all our
-- pages. Notice that @layout.html@ contains strings that look like @${title}@
-- and @${body}@. These are variable directives that we'll need to fill values
-- in for.
--
-- @index.markdown@ is a pretty basic Markdown file, and @style.scss@ is a
-- <http://sass-lang.com Scss> file.
--
-- Now let's look inside @Main.hs@:
--
-- > import Pencil
-- >
-- > config :: Config
-- > config =
-- >   (updateEnv (insertText "title" "My Simple Website") .
-- >    setSourceDir "examples/Simple/site/" .
-- >    setOutputDir "examples/Simple/out/") defaultConfig
-- >
-- > website :: PencilApp ()
-- > website = do
-- >   layout <- load toHtml "layout.html"
-- >   index <- load toHtml "index.markdown"
-- >   render (layout <|| index)
-- >
-- >   renderCss "style.scss"
-- >
-- > main :: IO ()
-- >   main = run website config
--
-- First, we need to set up a 'Config'. We start with 'defaultConfig', and
-- modify it slightly, specifying where the source files live, and where we want
-- the output files to go. We also add a @title@ variable with the value @"My
-- Simple Website"@ into the environment.
--
-- An 'Env', or environment, is just a mapping of variables to its values. A
-- variable can hold a string, number, boolean, date, and so forth. Once a
-- variable is defined, we can use that variable in our web pages via a
-- variable directive like @${title}@.
--
-- Let's now look at the @website@ function. Note that its type is @PencilApp
-- ()@. 'PencilApp' is the monad transformer that web pages are built under.
-- Don't worry if you aren't familiar with monad transformers; in simple terms,
-- @PencilApp@ is a function that takes a @Config@, and does all the source file
-- loading and web page rendering under the @IO@ monad. So @website@ is a
-- function that is waiting for a @Config@. We "give" @website@ a @Config@ with
-- this code, which is the @main@ function:
--
-- @
-- run website config
-- @
--
-- Now let's dissect the @website@ function itself. The first thing we do is
-- @'load' toHtml "layout.html"@, which loads our layout file into something
-- called a 'Page'. In short, a 'Page' holds the contents of the file, plus the
-- environment of that file, plus the final output destination of that file if
-- it is rendered. The 'toHtml' function tells 'load' that you want the output
-- file to have the @.html@ extension.
--
-- It's important to realize that 'toHtml' is not telling 'load' /how/ to load
-- @layout.html@; it's telling it what kind of file you want when you spit it
-- out. 'load' itself looks at the file extension to figure out that
-- @layout.html@ is an HTML file, and @index.markdown@ is a Markdown file. So we
-- use 'toHtml' when loading @index.markdown@ because we want the index page to
-- be rendered as an @.html@ file.
--
-- Now, what about @render (layout <|| index)@. What the heck is going on here?
-- In plain language, you can think of @(layout <|| index)@ as injecting the
-- contents of @index@ into @layout@. The way this works is that the contents of
-- @index@ is rendered (Markdown is converted to HTML, variable directives are
-- resolved through the given environment, etc) and then stuffed into a special
-- @body@ variable in @layout@'s environment. When @layout@ is rendered, the
-- variable directive @${body}@ in @layout@ is replaced with the contents of
-- @index@.
--
-- @(layout <|| index)@ describes what /will/ happen; it forms a 'Structure'.
-- Passing it into 'render' is what actually generates the web page.
--
-- Finally, we have @'renderCss' "style.scss"@, which is a helper method to load
-- and render CSS files in one step.
--
-- And that's it! If you run this code, it will spit out an @index.html@ file
-- and a @style.css@ file in the @examples\/Simple\/out\/@ folder.
--
-- To learn more, read through the documentation found in this module. To build
-- a blog, look at the Pencil.Blog module.

----------------------------------------------------------------------

-- $templates
--
-- Pencil comes with a simple templating engine. Templates allow us to build web
-- pages dynamically using Haskell code. This allows us to build modular
-- components. Templates can be used for things like shared page layouts,
-- navigation and blog post templates.
--
-- Pencil templates are regular text files that can contain a /preamble/ and
-- /directives/.
--
-- == Preamble
--
-- Preambles are YAML-formatted environment variable declarations inside your
-- source files. They should be declared at the top of the file, and you may
-- only have one preamble per source file. Example preamble, in the first part
-- of @my-blog-post.markdown@:
--
-- > <!--PREAMBLE
-- > postTitle: "Behind Python's unittest.main()"
-- > date: 2010-01-30
-- > tags:
-- >   - python
-- > -->
--
-- In the above example, Pencil will intelligently parse the @date@ value as a
-- `VDateTime`.
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
-- Pencil will throw an exception with some debugging information.
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
-- The @for@ directive allows us to loop over array type variable. This is
-- useful for things like rendering a list of blog post titles, and URLs to the
-- individual blog posts.
--
-- > <ul>
-- > ${for(posts)}
-- >   <li><a href="${this.url}">${postTitle}</a> - ${date}</li>
-- > ${end}
-- > </ul>
--
-- Assuming that @posts@ exists in our environment as an array of @Value@,
-- this will render each post's title, publish date, and will link it to
-- @this.url@. Note that inside the @for@ block, you have access to the current
-- environment's variables. This is why we're able to simply request
-- @${postTitle}@—it is the current post's @postTitle@ that will be rendered.
--
-- @this.url@ is a special variable that is automatically inserted for you
-- inside a loaded @Page@. It points to the final file path destination of that
-- current @Page@.
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
-- >   ${partial("partials/nav-bar.html")}
-- > ${end}

----------------------------------------------------------------------

-- $pagesStructuresResources
--
-- 'Page', 'Structure' and 'Resource' are the "big three" data types you need to
-- know to effectively use Pencil.

----------------------------------------------------------------------
