{-# LANGUAGE OverloadedStrings #-}

{-|
This module provides a standard way of building and generating blog posts.
Check out the Blog example
<https://github.com/elben/pencil/blob/master/examples/Blog/ here>. You can
also follow the [blogging tutorial
here](https://elbenshira.com/pencil/tutorials/03-blogging/).

To generate a blog for your website, first create a @blog/@ directory in
your web page source directory.

Then, name your blog posts in this format:

> yyyy-mm-dd-title-of-blog-post.markdown

Where @yyyy-mm-dd@ should be something like @2019-12-30@. This isn't used for
anything other than to keep each post ordered in the directory, for your ease
of viewing.

Each post is expected to have a preamble that has at least @postTitle@ and
@date@ defined. The date set in the preamble is used as the sort order of the
blog posts. The other variables are optional.

> <!--PREAMBLE
> postTitle: "The Meaning of Life"
> date: 2010-01-30
> draft: true
> tags:
>   - philosophy
> -->

You can mark a post as a draft via the @draft@ variable, so that it won't be
loaded when you call 'loadPosts'. You can also set the post's tags using,
as seen above in @tags@. Then, use 'loadPosts' to load the entire @blog/@
directory.

In the example below, @layout.html@ defines the outer HTML structure (with
global components like navigation), and @blog-post.html@ is a generic blog
post container that renders @${postTitle}@ as a header, @${date}@, and
@${body}@ for the post body.

@
layout <- 'load' "layout.html"
postLayout <- 'load' "blog-post.html"
posts <- 'loadPosts' "blog/"
render (fmap (layout <|| postLayout <|) posts)
@
-}
module Pencil.Blog
  ( loadPosts
  , postUrl
  , injectTitle

  -- * Tags
  -- | You can add tags to blog posts.
  , Tag
  , buildTagPages
  , buildTagPagesWith
  , injectTags
  ) where

import Pencil.Config
import Pencil.Env
import Pencil.App
import Pencil.Content
import Control.Monad (foldM)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.FilePath as FP

-- | Loads the given directory as a series of blog posts, sorted by the @date@
-- preamble environment variable. Posts with @draft: true@ are filtered out.
--
-- @
-- posts <- loadPosts "blog/"
-- @
loadPosts :: FilePath -> PencilApp [Page]
loadPosts fp = do
  posts <- loadDir False fp
  return $
    (filterByVar True "draft" (VBool True /=) . sortByVar "date" dateOrdering)
    (fmap (rename postUrl) posts)

-- | Rewrites file path for blog posts.
--
-- > postUrl "/blog/2011-01-01-post-title.html"
-- > -- "/blog/1-post-title.html/"
--
postUrl :: FilePath -> FilePath
postUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

-- | Given that the current @Page@ has a @postTitle@ in the environment, inject
-- the post title into the @title@ environment variable, prefixed with the given
-- title prefix.
--
-- This is useful for generating the @\<title\>${title}\</title\>@ tags in your
-- container layout.
--
-- For example, if the page's preamble has @postTitle: "The Meaning of Life"@,
-- then the snippet below will insert a @title@ variable with the value @"The
-- Meaning of Life - My Awesome Website"@:
--
-- @
-- injectTitle "My Awesome Website" post
-- @
--
injectTitle :: T.Text
            -- ^ Title prefix
            -> Page
            -> Page
injectTitle titlePrefix page =
  let title = case H.lookup "postTitle" (getPageEnv page) of
                       Just (VText t) -> T.append (T.append t " - ") titlePrefix
                       _ -> titlePrefix
      env' = insertText "title" title (getPageEnv page)
  in setPageEnv env' page

-- | Like, you know, a hashtag. Wraps a text.
type Tag = T.Text

-- | Finds all the tags from the given pages, and generates a page for each tag
-- found. Each tag page has a variable "posts" containing all pages that have
-- the tag.
--
-- Helper of 'buildTagPagesWith' defaulting to the variable name @posts@, and
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
--               (\\tag _ -> "blog\/tags\/" ++ 'Data.Text.unpack' tag ++ "\/")
--               posts
-- @
buildTagPagesWith :: FilePath
                  -- ^ Partial to load for the Tag index pages
                  -> T.Text
                  -- ^ Variable name inserted into Tag index pages for the list of
                  -- Pages tagged with the specified tag
                  -> (Tag -> FilePath -> FilePath)
                  -- ^ Function to generate the URL of the tag pages
                  -> [Page]
                  -> PencilApp (H.HashMap Tag Page)
buildTagPagesWith tagPageFp pagesVar fpf pages = do
  env <- asks getEnv

  let tagMap = groupByElements "tags" pages
  -- Build a mapping of tag to the tag list Page

  foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- rename (fpf tag) <$> load tagPageFp
      -- Generate the URL that this tag page will use.
      let url = T.pack $ "/" ++ getFilePath tagPage
      tagEnv <- (insertPages pagesVar taggedPosts . insertText "tag" tag . insertText "this.url" url . merge (getPageEnv tagPage)) env
      return $ H.insert tag (setPageEnv tagEnv tagPage) acc
    )
    H.empty
    (H.toList tagMap)

-- | Injects the tag map (usually generated by 'buildTagPages' or
-- 'buildTagPagesWith') into the page's environment as the variable @tags@,
-- which is an @VEnvList@.
injectTags :: H.HashMap Tag Page -> Page -> Page
injectTags tagMap page =
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
               else insert "tags" (VEnvList envs) (getPageEnv page)
  in setPageEnv env' page
