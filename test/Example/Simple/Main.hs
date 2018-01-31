{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil

config :: Config
config =
  (updateEnv (insertText "title" "My Simple Website") .
   setSourceDir "test/Example/Simple/site/" .
   setOutputDir "test/Example/Simple/out/") defaultConfig

website :: PencilApp ()
website = do
  layout <- load asHtml "layout.html"
  index <- load asHtml "index.markdown"
  render (layout <|| index)

  renderCss "style.scss"

main :: IO ()
main = run website config
