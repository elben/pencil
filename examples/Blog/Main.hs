{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Blog
import Control.Monad (forM_)
import qualified Data.Text as T

config :: Config
config =
  (updateEnv (insertText "title" "My Blog") .
   setSourceDir "examples/Blog/site/" .
   setOutputDir "examples/Blog/out/") defaultConfig

website :: PencilApp ()
website = do
  layout <- load toHtml "layout.html"

  postLayout <- load toHtml "post-layout.html"
  posts <- loadBlogPosts "blog/"
  tagPages <- buildTagPages "tag-list.html" "posts" (\tag _ -> "blog/tags/" ++ T.unpack tag ++ "/") posts
  let posts' = fmap ((layout <|| postLayout <|) . injectTagsEnv tagPages) posts
  forM_ posts' render

  env <- asks getEnv
  index <- load toHtml "index.html"
  let indexEnv = insertPages "posts" posts env
  withEnv indexEnv (render (layout <|| index))

main :: IO ()
main = run website config

