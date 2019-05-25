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
  , to
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
  --
  -- $environment

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
  , Reader.local

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
-- The best way to get started is to follow the tutorials and guides found on
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
-- The example above shows that Pencil allows you to easily load and process
-- Markdown files. You can then combine them with HTML layouts. Pencil also
-- supports processing SCSS files out-of-the-box. You can of course add new
-- functionality as required. If you run this code, it will spit out an
-- @index.html@ file and a @style.css@ file in the @examples\/Simple\/out\/@
-- folder.
--
-- To learn more, dig into the tutorials and guides found on
-- [elbenshira.com/pencil](elbenshira.com/pencil).

----------------------------------------------------------------------

-- $templates
--
-- Pencil comes with a simple templating engine. Templates allow us to build web
-- pages dynamically using Haskell code. Blog posts, for example, can share a
-- common HTML template.
--
-- Pencil's templating engine comes with variables, if blocks, for loops, and
-- partials. Read the
-- [Templates](https://elbenshira.com/pencil/guides/templates/) guide for a
-- thorough walk-through.
--
-- Example:
--
-- > <ul>
-- > ${for(posts)}
-- >   <li><a href="${this.url}">${postTitle}</a> - ${date}</li>
-- > ${end}
-- > </ul>

----------------------------------------------------------------------

-- $pagesStructuresResources
--
-- 'Page', 'Structure' and 'Resource' are the "big three" data types you need to
-- know to effectively use Pencil.
--
-- The [Pages and
-- Structures](https://elbenshira.com/pencil/guides/pages-and-structures/) guide
-- is the best starting point about these data types and their importance in
-- Pencil.

----------------------------------------------------------------------

-- $blogging
--
-- This module provides a standard way of building and generating blog posts.
-- Check out the Blog example
-- <https://github.com/elben/pencil/blob/master/examples/Blog/ here>. You can
-- also follow the [blogging tutorial
-- here](https://elbenshira.com/pencil/tutorials/03-blogging/).
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

-- $environment
--
-- The environment is where variables live. Composing web pages together
-- implies composing environments. This is where Pencil's power lies: in helping
-- you easily build the proper environment to render your web pages.
--
-- To get started, read the [environment
-- guide](https://elbenshira.com/pencil/guides/environment/).

----------------------------------------------------------------------