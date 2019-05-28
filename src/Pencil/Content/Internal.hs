{-# LANGUAGE DeriveGeneric #-}

{-|
Internal implementation for functions that content.
-}
module Pencil.Content.Internal where

import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import GHC.Generics (Generic)
import Pencil.Env.Internal
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as H
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified System.FilePath as FP

-- | The @Page@ is an important data type in Pencil.
--
-- Source files like Markdown and HTML are loaded (e.g. via 'Pencil.Content.load') as a @Page@.
-- A page contains the contents of the file, their un-evaluated [template
-- directives](https://elbenshira.com/pencil/guides/templates/) (e.g.
-- @${body}@), the variables defined in the preamble, and the destination file
-- path.
--
-- The contents /may/ be in its converted form. 'Pencil.Content.load' will convert Markdown to
-- HTML, for example.
--
-- Pages can be /combined/ together into a 'Structure', or inserted into the
-- environment (see 'Pencil.Content.insertPages'). But at the end of the day, even a structure
-- is converted back into a page on 'Pencil.Content.render'. This is because it is the page
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

-- | Gets the Env of a 'Page'.
getPageEnv :: Page -> Env
getPageEnv = pageEnv

-- | Sets the Env of a 'Page'.
setPageEnv :: Env -> Page -> Page
setPageEnv env p = p { pageEnv = env }

-- | Sets this 'Page' as the designated final 'FilePath'.
--
-- This is useful when you are building a 'Structure' but don't want the file
-- path of the last 'Page' in the structure to be the destination file path on
-- render.
--
-- The [Pages and
-- Structures](https://elbenshira.com/pencil/guides/pages-and-structures/) guide
-- describes this in detail.
--
-- @
-- a <- load "a.html"
-- b <- load "b.html"
-- c <- load "c.html"
--
-- -- Rendered file path is "c.html"
-- render $ a <|| b <| c
--
-- -- Rendered file path is "b.html"
-- render $ a <|| useFilePath b <| c
-- @
--
useFilePath :: Page -> Page
useFilePath p = p { pageUseFilePath = True }

-- | Sets this 'Page' to render with escaped XML/HTML tags.
--
-- This is useful when you are building an RSS feed, and you need the /contents/
-- of each item in the feed to HTML-escaped.
--
-- @
-- rss <- load "rss.xml"
-- item1 <- load "item1.html"
--
-- render $ rss <|| escapeXml item1
-- @
--
escapeXml :: Page -> Page
escapeXml p = p { pageEscapeXml = True }


-- | A @Structure@ is a list of 'Page's, defining a nesting order. Think of them
-- like <https://en.wikipedia.org/wiki/Matryoshka_doll Russian nesting dolls>.
-- The first element defines the outer-most container, and subsequent elements
-- are /inside/ the previous element.
--
-- You commonly use @Structure@s to insert a page containing content (e.g. a blog
-- post) into a container (e.g. a layout shared across all your web pages).
--
-- Build structures using 'Pencil.Content.struct', 'Pencil.Content.<||' and 'Pencil.Content.<|'.
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
-- When we 'Pencil.Content.render' @layout <|| index@, the contents of the index (and about)
-- page is injected into the layout page through the variable @${body}@. So
-- @layout.html@ must use @${body}@ somewhere in its own body.
--
-- Structures also control the closure of variables. Variables defined in a
-- page are accessible both by pages above and below. This allows inner
-- pages to define variables like the blog post title, which may be used in
-- the outer page to, say, set the @\<title\>@ tag.
--
-- In this way, structures allows efficient page reuse. See the private function
-- 'Pencil.Content.Internal.apply' to learn more about how structures are evaluated.
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
-- Use 'Pencil.Content.passthrough', 'Pencil.Content.loadResource' and
-- 'Pencil.Content.loadResources' to build a @Resource@ from a file.
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


-- | Enum for file types that can be parsed and converted by Pencil.
data FileType = Html
              | Markdown
              | Css
              | Sass
              | Other
  deriving (Eq, Generic)

-- | 'Hashable' instance of @FileType@.
instance Hashable FileType

-- | A 'H.HashMap' of file extensions (e.g. @markdown@) to 'FileType'.
--
-- * 'Html': @html, htm@
-- * 'Markdown': @markdown, md@
-- * 'Css': @css@
-- * 'Sass': @sass, scss@
--
fileTypeMap :: H.HashMap String FileType
fileTypeMap = H.fromList
  [ ("html", Html)
  , ("htm", Html)
  , ("markdown", Markdown)
  , ("md", Markdown)
  , ("css", Css)
  , ("sass", Sass)
  , ("scss", Sass)
  ]

-- | Mapping of 'FileType' to the final converted format. Only contains
-- 'FileType's that Pencil will convert.
--
-- * 'Markdown': @html@
-- * 'Sass': @css@
--
extensionMap :: H.HashMap FileType String
extensionMap = H.fromList
  [ (Markdown, "html")
  , (Sass, "css")]

-- | Class for types that has a final file path for rendering.
--
-- This allows file-path-changing methods to be re-used across 'Pencil.Content.Internal.Page',
-- 'Pencil.Content.Internal.Structure' and 'Pencil.Content.Internal.Resource' types.
class HasFilePath a where
  getFilePath :: a -> FilePath
  setFilePath :: FilePath -> a -> a

-- | Converts a 'FileType' into its converted webpage extension, if Pencil would
-- convert it (e.g. Markdown to HTML).
--
-- >>> toExtension Markdown
-- Just "html"
--
toExtension :: FileType -> Maybe String
toExtension ft = H.lookup ft extensionMap

-- | Takes a file path and returns the 'FileType', defaulting to 'Other' if it's
-- not a supported extension.
fileType :: FilePath -> FileType
fileType fp =
  -- takeExtension returns ".markdown", so drop the "."
  M.fromMaybe Other (H.lookup (map Char.toLower (drop 1 (FP.takeExtension fp))) fileTypeMap)

-- | Returns True if the file path is a directory.
-- Examples: foo/bar/
-- Examples of not directories: /foo, foo/bar, foo/bar.baz
isDir :: FilePath -> Bool
isDir fp = null (FP.takeBaseName fp)

-- | Replaces the file path's extension with @.html@.
--
-- @
-- rename toHtml \<$\> 'Pencil.Content.load' "about.htm"
-- @
--
toHtml :: FilePath -> FilePath
toHtml fp = FP.dropExtension fp ++ ".html"

-- | Converts a file path into a directory name, dropping the extension.
-- Pages with a directory as its file path is rendered as an index file in that
-- directory.
--
-- For example, @pages/about.html@ is transformed into @pages\/about\/@, which
-- upon 'Pencil.Content.render' results in the destination file path @pages\/about\/index.html@:
--
-- @
-- toDir "pages/about.html"
-- @
--
-- Load and render as @pages\/about\/@:
--
-- @
-- render $ 'rename' toDir \<$\> 'Pencil.Content.load' "pages/about.html"
-- @
--
toDir :: FilePath -> FilePath
toDir fp = FP.replaceFileName fp (FP.takeBaseName fp) ++ "/"

-- | Replaces the file path's extension with @.css@.
--
-- @
-- rename toCss \<$\> 'Pencil.Content.load' "style.sass"
-- @
--
toCss :: FilePath -> FilePath
toCss fp = FP.dropExtension fp ++ ".css"

-- | Converts file path into the expected extensions. This means @.markdown@
-- become @.html@, @.sass@ becomes @.css@, and so forth. See 'extensionMap' for
-- conversion table.
toExpected :: FilePath -> FilePath
toExpected fp = maybe fp ((FP.dropExtension fp ++ ".") ++) (toExtension (fileType fp))

-- | Transforms the file path.
--
-- @
-- about <- load "about.htm"
-- render $ struct (rename 'toHtml' about)
-- @
rename :: HasFilePath a => (FilePath -> FilePath) -> a -> a
rename f a = setFilePath (f (getFilePath a)) a

-- | Sets the target file path to the specified file path. If the given file path
-- is a directory, the file name set to @index.html@. If the file path is a file
-- name, then the file is renamed.
--
-- Move @stuff/about.html@ to @about/blah.html@ on render:
--
-- > about <- to "about/blah.html" <$> load "stuff/about.htm"
--
-- Convert the destination file path to @about/index.html@:
--
-- > about <- to "about/" <$> load "stuff/about.htm"
-- > render about
--
-- Equivalent to the above example:
--
-- > about <- load "stuff/about.htm"
-- > render $ to "about/" about
--
to :: HasFilePath a => FilePath -> a -> a
to = move' "index.html"

-- | Moves the target file path to the specified file path. Behaves similar to
-- the UNIX @mv@ command: if the given file path is a directory, the file name
-- is kept the same. If the file path is a file name, then the file is renamed.
--
-- Move @assets/style.css@ to @stylesheets/style.css@:
--
-- > move "stylesheets/" <$> load "assets/style.css"
--
-- Move @assets/style.css@ to @stylesheets/base.css@.
--
-- > move "stylesheets/base.css" <$> load "assets/style.css"
--
move :: HasFilePath a => FilePath -> a -> a
move fp a = move' (FP.takeFileName (getFilePath a)) fp a

-- | Internal implemenation for 'move' and 'to'.
--
-- Moves the target file path to the specified FilePath. If the given FilePath
-- is a directory, the file name is kept the same. If the FilePath is a file
-- name, then @fromFileName@ is used as the file name.
move' :: HasFilePath a => FilePath -> FilePath -> a -> a
move' fromFileName fp a =
  let fromFileName = FP.takeFileName (getFilePath a)
      toDir = FP.takeDirectory fp
      fp' = if isDir fp
              then toDir ++ "/" ++ fromFileName
              else toDir ++ "/" ++ FP.takeFileName fp
  in setFilePath fp' a