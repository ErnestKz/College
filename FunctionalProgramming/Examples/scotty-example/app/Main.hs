{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy
import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R


main = scotty 3000 $ do
  get "/" $ do
    html "Hello World!"

{-
  get "/greet/" $ do
      html  "Hello there"
-}

  get "/greet/:name" $ do
      name <- param "name"
      html $ response name

response :: Text -> Text
response n = do R.renderHtml $ do
                  H.h1 ( "Hello " >> H.toHtml n)

