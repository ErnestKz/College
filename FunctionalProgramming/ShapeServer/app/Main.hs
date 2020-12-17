{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy as T
import Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Base64.Lazy as B

import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import SampleDrawing (outputDrawing, exampleShape1String, exampleShape2String, exampleShape3String)

main = scotty 3000 $ do
  get "/" $ do
    html $ do 
      R.renderHtml $ do 
        H.head $ do
          H.link H.! A.rel "stylesheet" H.! A.href "styles.css"
        H.body $ do
          shapePicture
          H.p (H.toHtml $ "Shape 1: " ++  exampleShape1String)
          H.p (H.toHtml $ "Shape 2: " ++ exampleShape2String)
          H.p (H.toHtml $ "Shape 3: " ++ exampleShape3String)
          
  get "/styles.css" $ file "styles.css"
    
  
shapePicture :: H.Html
shapePicture = H.img H.! A.src ( H.stringValue ("data:image/png;base64," ++ (base64string))) H.! A.alt "picture"
  where base64string = C.unpack $ B.encode outputDrawing
