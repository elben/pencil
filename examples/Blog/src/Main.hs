{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Blog
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Control.Monad.Reader.Class

websiteTitle :: T.Text
websiteTitle = "My Blog"

config :: Config
config =
  (updateEnv (insertText "title" websiteTitle) .
   updateEnv (insertText "secret" "My global secret") .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/" .
  ) defaultConfig

website :: PencilApp ()
website = do
  layout <- load "layout.html"

  -- Load all our blog posts into `posts`, which is of type [Page]
  postLayout <- load "post-layout.html"
  posts <- loadBlogPosts "blog/"

  -- Build tag index pages. This is a map of a Tag (which is just text)
  -- to a Page which has the environment stuffed with blog posts that has that
  -- tag.
  tagPages <- buildTagPages "tag-list.html" posts

  -- Render all our blog posts. So for each post in posts, we:
  --
  -- - injectTitle - This generates a `title` variable in our env that has the
  --   format of "post title - My Blog"
  --
  -- - injectTagsEnv - This injects some environment variables so that our blog
  --   post page knows what tags it has (via `tags` variable), AND a reference to
  --   the tag index page that we built above (via `this.url`). Look through
  --   post-layout.html to see how to use these tag variables.
  --
  -- - (layout <|| postLayout <|) - We push the generated post Page into this
  --   Structure, so that all blog posts share the same postLayout.
  --
  -- - render - Finally we render all of our blog posts into files.
  --
  render $ fmap ((layout <|| postLayout <|) . injectTagsEnv tagPages . injectTitle websiteTitle)
                posts

  -- Build our index page.
  index <- load "index.html"
  render (layout <|| (useFilePath index) <<| coll "posts" posts)

  -- Render tag list pages. This is so that we can go to /blog/tags/awesome to
  -- see all the blog posts tagged with "awesome".
  render $ fmap (layout <||) (H.elems tagPages)

  -- Load RSS layout.
  rssLayout <- load "rss.xml"

  -- Build RSS feed of the last 10 posts.
  -- TODO elaborate.
  let rssFeedStruct = struct (useFilePath rssLayout) <<| coll "posts" (fmap escapeXml (take 10 posts))

  -- Render the RSS feed. We need to render inside a modified environment, where
  -- @toTextRss@ is used as the render function, so that dates are rendered in
  -- the RFC 822 format, per the RSS specification.
  local (setDisplayValue toTextRss)
        (render rssFeedStruct)

main :: IO ()
main = run website config

