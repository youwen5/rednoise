{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Control.Monad (forM_)
import Data.ByteString.Lazy
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

test :: String -> Html
test pageBody = docTypeHtml $ do
  H.head $ do
    H.title "Test Page"
  body $ do
    p $ preEscapedToHtml pageBody
