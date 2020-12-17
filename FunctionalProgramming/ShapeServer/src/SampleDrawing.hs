module SampleDrawing (outputDrawing, exampleDrawing, exampleShape1String, exampleShape2String, exampleShape3String) where

import Shapes
import Render (render,renderByteString,defaultWindow)
import Codec.Picture 
exampleDrawing =
  [
    (
      (scale (point 0.5 0.5) <+> translate (point (0) (0)))
     , (ellipse (point 1 0) (point (-2) (0)) 3.5)
     , (PixelRGB8 123 123 0)
     , (3)
     )
  ,(
      (rotate (0.4) <+> scale (point 2 2) <+> translate (point (0) (-0.2)))
   ,(polygon
     [
       (point (0) (0))
     , (point 0.2 0)
     , (point 0.2 (0.2))
     , (point 0.1 (0.3))
     , (point 0 2)
     ])
   , (PixelRGB8 0 123 123)
   , (4)
   )  
  , (
      (rotate (0.8) <+> scale (point 0.4 0.4) <+> translate (point (-2) (0.2)))
    , (rectangle 0.2)
    , (PixelRGB8 220 123 123)
    , (5)
      )  
  ]

exampleShape1String="((scale(point 0.5 0.5)<+>translate(point(0)(0))),(ellipse(point 1 0)(point(-2)(0))3.5),(Pixel RGB8 123 123 0),(3))"
exampleShape2String="((rotate(0.4)<+>scale(point 2 2)<+>translate(point(0)(-0.2))),(polygon[(point(0)(0)),(point 0.2 0),(point 0.2 (0.2)),(point 0.1 (0.3)),(point 0 2)]),(PixelRGB8 0 123 123),(4))"
exampleShape3String="((rotate(0.8)<+>scale(point 0.4 0.4)<+>translate(point(-2)(0.2))),(rectangle 0.2),(PixelRGB8 220 123 123),(5))"

outputDrawing = renderByteString defaultWindow exampleDrawing
