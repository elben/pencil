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

  , Structure
  , (<||)
  , (<|)
  , structure

  , Resource
  , loadResource
  , loadResources
  , passthrough
  , listDir

  , Render(..)
  , asHtml
  , asDir
  , asCss
  , asIntended

  -- * Environment Manipulation

  , merge
  , insertEnv
  , insertText
  , insertPages
  , updateEnvVal
  , sortByVar
  , filterByVar
  , groupByElements

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

  -- * Utils and re-exports

  , FileType
  , fileType
  , toExtension

  -- Re-exports
  , Reader.asks

  -- * Error handling

  , PencilException

  ) where

import Pencil.Internal

import Control.Monad.Reader as Reader

----------------------------------------------------------------------

-- $gettingstarted
--
-- We'll start by building a very simple website, with only a couple of pages,
-- to give you a feel for using Pencil. Go to http://elbenshira.com/pencil for
-- in-depth tutorials, including how you can set up a blog.
--
-- To build our simple website, we'll first create some folders and files:
--
-- > cd ~/code/mywebsite
-- > mkdir site/
-- > echo '
-- > <!DOCTYPE html>
-- > <html>
-- >   <head>
-- >     <title>${title}</title>
-- >     <link rel="stylesheet" href="style.css"/>
-- >   </head>
-- > <body>${body}</body>
-- > </html>' > site/layout.html
-- > echo 'Welcome to my *awesome* [website](http://example.com)!' > site/index.markdown
-- > echo '$mycolor: #ff0000; body { color: $mycolor; }' > site/style.scss
--
-- Notice that @layout.html@ contains two variable directives, @${title}@ and
-- @${body}@, which we will have to fill values for before rendering the pages that
-- use @layout.html@.
--
-- Then inside your @Main.hs@:
--
-- @
-- import Pencil
--
-- config :: Config
-- config = insertText "title" "My Website" defaultConfig
--
-- website :: PencilApp ()
-- website = do
--   layout <- load asHtml "layout.html"
--   index <- load asHtml "index.markdown"
--
--   render (layout <|| index)
--   renderCss "style.scss"
--
-- main = run website config
-- @

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
