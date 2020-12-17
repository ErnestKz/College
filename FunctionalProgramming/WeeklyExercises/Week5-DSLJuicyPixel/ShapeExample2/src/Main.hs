module Main where

import Shapes
import Render (render,defaultWindow)

exampleDrawing =  [ (scale (point 0.5 0.25) <+> translate (point (-1.5) (-1.5)), square) ]

main = render "output.png" defaultWindow exampleDrawing
