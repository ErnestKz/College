module Main where

import Shapes
import Render (render,defaultWindow)
import Codec.Picture 
exampleDrawing =
  [
    ((  scale (point 0.5 0.5) <+> translate (point (0) (0)) ),
     (ellipse (point 1 0) (point (-2) (0)) 3.5),
     (PixelRGB8 123 123 0),
     (3)
    ),
    (( rotate (0.4) <+> scale (point 2 2) <+> translate (point (0) (0)) ),

     

      (polygon
       [
         (point (0) (0))
       , (point 0.2 0)
       , (point 0.2 (0.2))
       , (point 0.1 (0.3))
       , (point 0 0.2)
       ]
      ),


     
     (PixelRGB8 0 123 123),
     (2)
    )
  ]

main = render "output.png" defaultWindow exampleDrawing
