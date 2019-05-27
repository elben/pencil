module Pencil.Files
  ( module Pencil.Files.Internal
  , toExtension
  , fileType
  , isDir

  , toHtml
  , toDir
  , toCss
  , toExpected

  , rename
  , to
  , move
  ) where

import Pencil.Files.Internal

import qualified Data.HashMap.Strict as H
import qualified Data.Maybe as M
import qualified System.FilePath as FP
import qualified Data.Char as Char

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
-- rename toHtml \<$\> 'load' "about.htm"
-- @
--
toHtml :: FilePath -> FilePath
toHtml fp = FP.dropExtension fp ++ ".html"

-- | Converts a file path into a directory name, dropping the extension.
-- Pages with a directory as its file path is rendered as an index file in that
-- directory.
--
-- For example, @pages/about.html@ is transformed into @pages\/about\/@, which
-- upon 'render' results in the destination file path @pages\/about\/index.html@:
--
-- @
-- toDir "pages/about.html"
-- @
--
-- Load and render as @pages\/about\/@:
--
-- @
-- render $ 'rename' toDir \<$\> 'load' "pages/about.html"
-- @
--
toDir :: FilePath -> FilePath
toDir fp = FP.replaceFileName fp (FP.takeBaseName fp) ++ "/"

-- | Replaces the file path's extension with @.css@.
--
-- @
-- rename toCss \<$\> 'load' "style.sass"
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
