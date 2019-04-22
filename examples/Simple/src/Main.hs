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
  layout <- load toHtml "layout.html"
  index <- load toHtml "index.markdown"
  render (layout <|| index)

  renderCss "stylesheet.scss"

main :: IO ()
main = run website config
