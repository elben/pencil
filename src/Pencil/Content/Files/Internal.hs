{-# LANGUAGE DeriveGeneric #-}

module Pencil.Content.Files.Internal where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import qualified Data.HashMap.Strict as H

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
-- This allows file-path-changing methods to be re-used across 'Page',
-- 'Structure' and 'Resource' types.
class HasFilePath a where
  getFilePath :: a -> FilePath
  setFilePath :: FilePath -> a -> a
