{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil

config :: Config
config =
  (updateEnv (insertText "title" "Pencil Documentation") .
   setSourceDir "examples/Docs/site/" .
   setOutputDir "docs/")
   defaultConfig

website :: PencilApp ()
website = do
  loadAndRender "default.scss"
  loadAndRender "tutorials/images/"
  loadAndRender "guides/images/"

  layout <- load "layout.html"

  index <- load "index.markdown"
  render (layout <|| index)

  tuts <- loadDir False "tutorials/"
  renderDir layout tuts

  guides <- loadDir False "guides/"
  renderDir layout guides

  where
    renderDir layout pages = render $ fmap ((layout <||) . rename toDir) pages


main :: IO ()
main = run website config
