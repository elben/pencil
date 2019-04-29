{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Blog
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

import Control.Monad.Reader.Class
import Control.Monad.IO.Class

import Debug.Trace

websiteTitle :: T.Text
websiteTitle = "My Blog"

config :: Config
config =
  (updateEnv (insertText "title" websiteTitle) .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/" .
   setRootUrl "https://example.com"
  ) defaultConfig

website :: PencilApp ()
website = do
  layout <- load toHtml "layout.html"

  -- Load all our blog posts into `posts`, which is of type [Page]
  postLayout <- load toHtml "post-layout.html"
  posts <- loadBlogPosts "blog/"
  -- rssStuff <- toRSS "My RSS FEED!!" "http://awesome.com/rss.xml" posts
  liftIO $ putStrLn "wohoo!"
  -- liftIO $ putStrLn (T.unpack rssStuff)

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

  -- Build our index page. Insert the blog posts into the env, so that we can
  -- render the list of blog posts. `withEnv` tells Pencil to use the modified
  -- environment when rendering the index page, since that's the env that has
  -- the list of blog posts.
  index <- load toHtml "index.html"
  env <- asks getEnv
  indexEnv <- insertPages "posts" posts env
  withEnv indexEnv (render (layout <|| index))

  liftIO $ putStrLn "!!! RSS !!!"

  rssLayout <- load id "rss.xml"
  -- TODO this only inserts the env, not the actual body. Where is the body? It's in the nodes, not
  -- the env. So how do I get to it and insert it into the env?
  -- How would I even do a "snippet" HTML page? I can't today. I need to rethink this.
  -- What if we merge body into the env? The "contents" can be thought of a variable
  -- with an *implicit* variable name of "body"
  -- Oh wait, we already do this. See apply function. So why isn't it here?
  -- rssEnv <- insertPagesEscape "posts" (take 10 (traceShowId posts)) env
  -- TODO confusingly i could call "render rssLayout" without a structure and it compiles.
  -- Mauybe I shouldn't use the render typeclass? When would a user need to render just a Page?
  -- We should have a diff method that means "we're gonna write something to a
  -- file"

  -- local (setDisplayValue toTextRss) (withEnv (traceShowId rssEnv) (render (structure rssLayout)))
  local (setDisplayValue toTextRss) (render (structure (useFilePath rssLayout) <<| coll "posts" (fmap escapeXml posts)))

  -- Render tag list pages. This is so that we can go to /blog/tags/awesome to
  -- see all the blog posts tagged with "awesome".
  render $ fmap (layout <||) (H.elems tagPages)

main :: IO ()
main = run website config

