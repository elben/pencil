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
    module Pencil.Content

  -- * Blogging
  --
  -- $blogging
  , module Pencil.Blog

  -- * Environment Manipulation
  --
  -- $environment
  , module Pencil.Env

  -- * Configuration

  , module Pencil.Config

  -- * The Pencil Type
  -- | Provides the main Pencil type that everything runs in.
  , module Pencil.App

  -- * Re-exports
  , Reader.asks
  , Reader.local
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
-- partials. Read the [templates
-- guide](https://elbenshira.com/pencil/guides/templates/) for a thorough
-- walk-through.
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
-- This module provides out-of-the-box blogging functionality. Read through the
-- [blogging tutorial](http://elbenshira.com/pencil/tutorials/03-blogging/) to
-- learn how to use it.

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