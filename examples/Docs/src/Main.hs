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

    layout <- load "layout.html"

    index <- load "index.markdown"
    render (layout <|| index)

    t1 <- load "tutorials/01-getting-started.markdown"
    render (layout <|| rename toDir t1)

    t2 <- load "tutorials/02-deploying-to-github-pages-using-circle.markdown"
    render (layout <|| rename toDir t2)

    t3 <- load "tutorials/03-blogging.markdown"
    render (layout <|| rename toDir t3)

main :: IO ()
main = run website config
