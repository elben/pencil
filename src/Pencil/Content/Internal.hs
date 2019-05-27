{-# LANGUAGE DeriveGeneric #-}

module Pencil.Content.Internal where

import Pencil.Env.Internal
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import qualified Data.Text as T

-- | The @Page@ is an important data type in Pencil.
--
-- Source files like Markdown and HTML are loaded (e.g. via 'load') as a @Page@.
-- A page contains the contents of the file, their un-evaluated [template
-- directives](https://elbenshira.com/pencil/guides/templates/) (e.g.
-- @${body}@), the variables defined in the preamble, and the destination file
-- path.
--
-- The contents /may/ be in its converted form. 'load' will convert Markdown to
-- HTML, for example.
--
-- Pages can be /combined/ together into a 'Structure', or inserted into the
-- environment (see 'insertPages'). But at the end of the day, even a structure
-- is converted back into a page on 'render'. This is because it is the page
-- that is finally rendered into an actual web page when you run your program.
--
data Page = Page
  { pageEnv        :: Env

  , pageFilePath :: FilePath
  -- ^ The rendered output path of this page. Defaults to the input file path.
  -- This file path is used to generate the self URL that is injected into the
  -- environment.

  , pageUseFilePath :: Bool
  -- ^ Whether or not this Page's URL should be used as the final URL in the
  -- render.

  , pageEscapeXml :: Bool
  -- ^ Whether or not XML/HTML tags should be escaped when rendered.
  } deriving (Eq, Show)

-- | A @Structure@ is a list of 'Page's, defining a nesting order. Think of them
-- like <https://en.wikipedia.org/wiki/Matryoshka_doll Russian nesting dolls>.
-- The first element defines the outer-most container, and subsequent elements
-- are /inside/ the previous element.
--
-- You commonly use @Structure@s to insert a page containing content (e.g. a blog
-- post) into a container (e.g. a layout shared across all your web pages).
--
-- Build structures using 'struct', '<||' and '<|'.
--
-- @
-- layout <- load "layout.html"
-- index <- load "index.markdown"
-- about <- load "about.markdown"
-- render (layout <|| index)
-- render (layout <|| about)
-- @
--
-- In the example above we load a layout page, which defines the outer HTML
-- structure like @\<html\>\<\/html\>@. We then "push" the index page and the
-- about page into the layout.
--
-- When we 'render' @layout <|| index@, the contents of the index (and about)
-- page is injected into the layout page through the variable @${body}@. So
-- @layout.html@ must use @${body}@ somewhere in its own body.
--
-- Structures also control the closure of variables. Variables defined in a
-- page are accessible both by pages above and below. This allows inner
-- pages to define variables like the blog post title, which may be used in
-- the outer page to, say, set the @\<title\>@ tag.
--
-- In this way, structures allows efficient page reuse. See the private function
-- 'apply' to learn more about how structures are evaluated.
--
-- /The Default File Path Rule/. When a structure is rendered, the /last/
-- non-collection page in the structure is used as the destination file path.
-- You can select a different page via 'useFilePath'.
--
-- The [Pages and
-- Structures](https://elbenshira.com/pencil/guides/pages-and-structures/) guide
-- also describes structures in detail.
--
-- Note that structures differ from the @${partial(...)}@ directive, which has no
-- such variable closures. The partial directive is much simpler—think of them
-- as copy-and-pasting snippets from one file to another. A partial has
-- the same environment as the context in which the partial directive appears.
--
--
data Structure = Structure
  { structureNodes :: NonEmpty Node
  , structureFilePath :: FilePath
  , structureFilePathFrozen :: Bool
  -- ^ True if the file path should no longer be changed. This happens when a
  -- page with `useFilePath = True` was pushed into the structure.
  }

-- | An inner element in the Structure. Either a singular Page, or a collection
-- of Pages. The Text element is the variable name that the inner page's content
-- is injected as. Defaults to @"body"@.
data Node =
    Node T.Text Page
  | Nodes T.Text [Page]

-- | @Resource@ is used to copy static binary files to the destination, and to
-- load and render files that just needs conversion without template directives
-- or structures.
--
-- This is how Pencil handles files like images, compiled JavaScript, or text
-- files that require only a straight-forward conversion.
--
-- Use 'passthrough', 'loadResource' and 'loadResources' to build a @Resource@
-- from a file.
--
-- In the example below, @robots.txt@ and everything in the @images/@ directory
-- will be rendered as-is.
--
-- @
-- passthrough "robots.txt" >>= render
-- passthrough "images/" >>= render
-- @
--
data Resource
  = Single Page
  | Passthrough FilePath FilePath
  -- ^ in and out file paths (can be dir or files)
