{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil

config :: Config
config =
  (updateEnv (insertText "title" "My Awesome Website") .
   setSourceDir "examples/Simple/site/" .
   setOutputDir "examples/Simple/out/")
   defaultConfig

website :: PencilApp ()
website = do
  layout <- load "layout.html"
  index <- load "index.markdown"
  render (layout <|| index)

  loadAndRender "stylesheet.scss"

main :: IO ()
main = run website config
