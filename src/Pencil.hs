{-|
Main Pencil module. This module re-exports most functions, so you should only need to import this one.
-}
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

    Render(..)

  -- ** Page

  , Page
  , load
  , loadDir
  , loadAndRender
  , rename
  , to
  , move
  , useFilePath
  , escapeXml
  , getPageEnv, setPageEnv

  -- ** Structure

  , Structure
  , struct
  , (<||)
  , (<|)
  , (<<|)
  , coll

  -- ** Resource

  , Resource
  , passthrough
  , loadResource
  , loadResources

  -- ** Utilities

  , listDir
  , toExpected
  , toHtml
  , toCss
  , toDir

  -- * Blogging
  --
  -- $blogging

  , loadPosts
  , postUrl
  , injectTitle
  , Tag
  , buildTagPages
  , buildTagPagesWith
  , injectTags

  -- * Environment Manipulation
  --
  -- $environment

  , merge
  , adjust
  , insert
  , insertText
  , insertPages
  , sortByVar
  , filterByVar
  , groupByElements
  , toTextRss
  , withEnv

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

  -- * Utilities and Re-exports

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

import Pencil.App
import Pencil.Blog
import Pencil.Config
import Pencil.Content
import Pencil.Env

import Control.Monad.Reader as Reader

----------------------------------------------------------------------

-- $gettingstarted
--
-- Pencil helps you build static websites in Haskell. Write your website's
-- content in HTML or Markdown, and use the power of Pencil to compose
-- components together into HTML pages.
--
-- The best way to get started is to follow the tutorials and guides found at
-- [elbenshira.com/pencil](https://elbenshira.com/pencil).
--
-- Here is a simple website of a couple of pages, based off
-- [this](https://github.com/elben/pencil/blob/master/examples/Simple/src/Main.hs)
-- example:
--
-- > module Main where
-- > 
-- > import Pencil
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
-- > main = run website defaultConfig
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
-- introduces these important data types. The documentation found here goes into
-- further detail.

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
-- loaded when you call 'loadPosts'. You can also set the post's tags using,
-- as seen above in @tags@. Then, use 'loadPosts' to load the entire @blog/@
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
-- posts <- 'loadPosts' "blog/"
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