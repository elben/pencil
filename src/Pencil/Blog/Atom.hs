{-# LANGUAGE OverloadedStrings #-}

module Pencil.Blog.Atom where

import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export
import qualified Text.XML.Light.Output as XML

examplePosts :: [(String, String, String)] -- Date, URL, Content
examplePosts =
  [ ("2000-02-02T18:30:00Z", "http://example.com/2", "Bar.")
  , ("2000-01-01T18:30:00Z", "http://example.com/1", "Foo.")
  ]


toEntry :: (String, String, String) -> Atom.Entry
toEntry (date, url, content) =
  (Atom.nullEntry
     url -- The ID field. Must be a link to validate.
     (Atom.TextString content) -- Title
     date)
  { Atom.entryAuthors = [Atom.nullPerson {Atom.personName = "J. Smith"}]
  , Atom.entryLinks = [Atom.nullLink url]
  , Atom.entryContent = Just (Atom.HTMLContent content)
  }
