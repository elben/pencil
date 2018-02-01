{-# LANGUAGE OverloadedStrings #-}

module Pencil.Blog
  (
    -- * Getting started
    --
    -- $gettingstarted
    --
    loadBlogPosts
  , blogPostUrl
  , injectTitle
  , buildTagPagesWith
  , buildTagPages
  , injectTagsEnv
  ) where

import Pencil
import Pencil.Internal.Env
import Control.Monad (liftM, foldM)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.FilePath as FP

-- $gettingstarted
--
-- This module provides a standard way of building and generating blog posts.
-- Check out the Blog example
-- <https://github.com/elben/pencil/blob/master/examples/Blog/ here>.
--
-- To generate a blog for your website, first create a @blog/@ directory in
-- your web page source directory.
--
-- Then, name your blog posts in this format:
--
-- > yyyy-mm-dd-title-of-blog-post.markdown
--
-- The files in that directory are expected to have preambles that have at
-- least @postTitle@ and @date@ defined. The other ones are optional.
--
-- > <!--PREAMBLE
-- > postTitle: "Behind Python's unittest.main()"
-- > date: 2010-01-30
-- > draft: true
-- > tags:
-- >   - python
-- > -->
--
-- You can mark a post as a draft via the @draft@ variable (it won't be
-- loaded when you call 'loadBlogPosts'), and add tagging (see below) via
-- @tags@. Then, use 'loadBlogPosts' to load the entire @blog/@ directory.
--
-- In the example below, @layout.html@ defines the outer HTML structure (with
-- global components like navigation), and @blog-post.html@ is a generic blog
-- post container that renders @${postTitle}@ as a header, @${date}@, and
-- @${body}@ for the post body.
--
-- @
-- layout <- 'load' toHtml "layout.html"
-- postLayout <- 'load' toHtml "blog-post.html"
-- posts <- 'loadBlogPosts' "blog/"
-- render (fmap (layout <|| postLayout <|) posts)
-- @
--

-- | Loads the given directory as a series of blog posts, sorted by the @date@
-- PREAMBLE environment variable. Posts with @draft: true@ are filtered out.
--
-- @
-- posts <- loadBlogPosts "blog/"
-- @
loadBlogPosts :: FilePath -> PencilApp [Page]
loadBlogPosts fp = do
  -- Load posts
  postFps <- listDir False fp

  -- Sort by date (newest first) and filter out drafts
  liftM (filterByVar True "draft" (VBool True /=) . sortByVar "date" dateOrdering)
        (mapM (load blogPostUrl) postFps)

-- | Rewrites file path for blog posts.
-- @\/blog\/2011-01-01-the-post-title.html@ => @\/blog\/the-post-title\/@
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

-- | Given that the current @Page@ has a @postTitle@ in the environment, inject
-- the post title into the @title@ environment variable, prefixed with the given
-- title prefix.
--
-- This is useful for generating the @\<title\>${title}\</title\>@ tags in your
-- container layout.
--
-- @
-- injectTitle "My Awesome Website" post
-- @
--
-- The above example may insert a @title@ variable with the value @"How to do X
-- - My Awesome Website"@.
--
injectTitle :: T.Text
            -- ^ Title prefix.
            -> Page
            -> Page
injectTitle titlePrefix page =
  let title = case H.lookup "postTitle" (getPageEnv page) of
                       Just (VText t) -> T.append (T.append t " - ") titlePrefix
                       _ -> titlePrefix
      env' = insertText "title" title (getPageEnv page)
  in setPageEnv env' page

type Tag = T.Text

-- | Helper of 'buildTagPagesWith' defaulting to the variable name @posts@, and
-- the tag index page file path @blog\/tags\/my-tag-name\/@.
--
-- @
-- tagPages <- buildTagPages pages
-- @
--
buildTagPages :: FilePath
              -> [Page]
              -> PencilApp (H.HashMap Tag Page)

buildTagPages tagPageFp =
  buildTagPagesWith
    tagPageFp
    "posts"
    (\tag _ -> "blog/tags/" ++ T.unpack tag ++ "/")

-- | Build the tag index pages.
--
-- Given blog post @Page@s with @tags@ variables in its PREAMBLE, builds @Page@s that
-- contain in its environment the list of @Page@s that were tagged with that
-- particular tag. Returns a map of tag of the tag index page.
--
-- @
-- tagPages <- buildTagPagesWith
--               "tag-list.html"
--               "posts"
--               (\tag _ -> "blog/tags/" ++ 'Data.Text.unpack' tag ++ "/")
--               posts
-- @
buildTagPagesWith :: FilePath
                  -- ^ Partial to load for the Tag index pages
                  -> T.Text
                  -- ^ Variable name inserted into Tag index pages for the list of
                  -- Pages tagged with the specified tag
                  -> (Tag -> FilePath -> FilePath)
                  -- ^ Function to generate the URL of the tag pages.
                  -> [Page]
                  -> PencilApp (H.HashMap Tag Page)
buildTagPagesWith tagPageFp pagesVar fpf pages = do
  env <- asks getEnv

  let tagMap = groupByElements "tags" pages
  -- Build a mapping of tag to the tag list Page

  foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- load (fpf tag) tagPageFp
      let tagEnv = (insertPages pagesVar taggedPosts . insertText "tag" tag . merge (getPageEnv tagPage)) env
      return $ H.insert tag (setPageEnv tagEnv tagPage) acc
    )
    H.empty
    (H.toList tagMap)

-- | Inject the given tagging map into the given @Page@'s environment, as the
-- @tags@ variable, whose value is a @VEnvList@.
injectTagsEnv :: H.HashMap Tag Page -> Page -> Page
injectTagsEnv tagMap page =
  -- Build up an env list of tag to that tag page's env. This is so that we can
  -- have access to the URL of the tag index pages.
  let envs =
        case H.lookup "tags" (getPageEnv page) of
          Just (VArray tags) ->
              L.foldl'
                (\acc envData ->
                  case envData of
                    VText tag ->
                      case H.lookup tag tagMap of
                        Just tagIndexPage -> getPageEnv tagIndexPage : acc
                        _ -> acc
                    _ -> acc)
                [] tags
          _ -> []

      -- Overwrite the VArray "tags" variable in the post Page with VEnvList of the
      -- loaded Tag index pages. This is so that when we render the blog posts, we
      -- have access to the URL of the Tag index.
      env' = if null envs
               then getPageEnv page
               else insertEnv "tags" (VEnvList envs) (getPageEnv page)
  in setPageEnv env' page

