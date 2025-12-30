{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

test :: Html
test = docTypeHtml $ do
  H.head $ do
    H.title "Test Page"
  body $ do
    p "A test page in pure Haskell"
