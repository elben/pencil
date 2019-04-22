{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Blog
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

websiteTitle :: T.Text
websiteTitle = "My Blog"

config :: Config
config =
  (updateEnv (insertText "title" websiteTitle) .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/") defaultConfig

website :: PencilApp ()
website = do
  layout <- load toHtml "layout.html"

  -- Load all our blog posts into `posts`, which is of type [Page]
  postLayout <- load toHtml "post-layout.html"
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
  --   structure, so that all blog posts share the same postLayout.
  --
  -- - render - Finally we render all of our blog posts into files.
  --
  render $ fmap ((layout <|| postLayout <|) . injectTagsEnv tagPages . injectTitle websiteTitle)
                posts

  -- Build our index page. Insert the blog posts into the env, so that we can
  -- render the list of blog posts. `withEnv` tells Pencil to use the modified
  -- environment when rendering the index page, since that's the env that has
  -- the list of blog posts.
  index <- load toHtml "index.html"
  env <- asks getEnv
  let indexEnv = insertPages "posts" posts env
  withEnv indexEnv (render (layout <|| index))

  -- Render tag list pages. This is so that we can go to /blog/tags/awesome to
  -- see all the blog posts tagged with "awesome".
  render $ fmap (layout <||) (H.elems tagPages)

main :: IO ()
main = run website config

