module Example where

import Shapes
import Signal
import Animate (animate)
import Render (defaultWindow)

staticBall :: Signal Drawing
staticBall = pure ball
     where ball = [(scale (point 0.5 0.5) <+> translate (point 1.2 0.4), circle)]

rotatingSquare :: Signal Drawing
rotatingSquare = fmap (:[]) $ fmap sq rs --  constS [( scale (point 0.5 0.5) <+> translate (point 1.2 0.4), square)] -- mapS (:[]) $ mapS sq rs
     where            
           rs :: Signal Transform
           rs = fmap rotate timeS -- using timeS as the source for the rotation angle

           sqs :: Transform -> Signal (Transform,Shape)
           sqs t = fmap sq rs

           sq :: Transform -> (Transform, Shape)
           sq t = ( scale (point 0.5 0.5) <+> translate (point 1.2 0.4) <+> t, square)

{-
movingBall :: Signal Drawing
movingBall = fmap (:[]) $ pure ball <*> fmap translate pos
       where
             
             ts :: Signal Transform
             ts = fmap translate posS

             bounceY :: Signal Double
             bounceY = fmap (sin . (3*)) timeS

             posS :: Signal Point
             posS = pure point <*> pure 0.0 <*> bounceY

             ball :: Transform -> (Transform, Shape)
             ball t = ( t <+> scale (point 0.3 0.3), circle )
             -}
             
movingBall :: Signal Drawing
movingBall = fmap (:[]) $ fmap (addT ball) ts
  where addT (ts,s) t = (ts <+> t, s)
        ball = (scale (point 0.3 0.3), circle)
        ts = fmap translate posS
        posS = pure point <*> pure 0.0 <*> bounceY
        bounceY = fmap (sin . (*3)) timeS


movingBall2 :: Signal Drawing
movingBall2 = drawingS --fmap (:[]) $ fmap (addT ball) tsS
  where

        addT :: Transform -> (Transform,Shape) -> (Transform,Shape)
        addT t (ts,s) = (ts <+> t, s)

        ball :: (Transform, Shape)
        ball = (scale (point 0.3 0.3), circle)
             
        -- Signal of transformed drawings
        drawingS :: Signal Drawing
        drawingS = fmap (:[]) movingShapeS

        -- Signal of transformed shapes
        movingShapeS :: Signal (Transform, Shape)
        movingShapeS = ats <*> shapeS

        -- Signal of shapes, used to make signal of drawings
        shapeS :: Signal (Transform, Shape)
        shapeS = pure ball
        
        -- Signal of shape-transforming functions
        ats :: Signal ( (Transform,Shape) -> (Transform,Shape) )
        ats = fmap addT tsS

        -- Signal of transforms, used to make signal of shapes
        tsS :: Signal Transform
        tsS = fmap translate posS
        
        -- Signal of Points, used to make signal of transforms
        posS :: Signal Point
        posS = pure point <*> pure 0.0 <*> bounceY

        -- signal of Y-positions, used to make up signal of points
        bounceY :: Signal Double
        bounceY = fmap (sin . (*3)) timeS

bouncingBall :: Signal Drawing
bouncingBall = fmap (:[]) $ fmap ball ( fmap translate pos )
       where bounceY = fmap (sin . (3*)) timeS
             bounceX = fmap (sin . (2*)) timeS
             pos = pure point <*> bounceX <*> bounceY
             ball t = ( t <+> scale (point 0.3 0.3), circle )

joinDS :: Signal [a] -> Signal [a] -> Signal [a]
joinDS s0 s1 = (fmap ( (++) ) s0) <*> s1


--example = staticBall
example = bouncingBall `joinDS` rotatingSquare
--example = movingBall2

           
runExample :: IO ()
runExample = animate defaultWindow 0 endTime example
  where endTime = 15

