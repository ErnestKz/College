module Main (main) where

import Criterion.Main
import SampleDrawing (exampleDrawing)
import Render (render, renderByteString, oldRenderByteString, defaultWindow)

main :: IO ()
main = defaultMain
  [
    bgroup "rendering and storing output"
    [
      bench "Just bytestring" $ nf (renderByteString defaultWindow) exampleDrawing
    , bench "Writing bytestring to disk" $ nfIO (render "bench.png" defaultWindow exampleDrawing)
    ]
  , bgroup "pixel renderer"
    [
      bench "default inside" $  nf (oldRenderByteString defaultWindow) exampleDrawing
    , bench "faster inside " $  nf (renderByteString defaultWindow) exampleDrawing
    ]
  ]
