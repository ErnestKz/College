import Codec.Picture

main :: IO ()
main = writePng "./mandel.png" $ generateImage pixRenderer winW winH
  where
    pixRenderer x y = PixelRGB8 40 10 (pointColour $ pixelToPoint (x, y))

-- Size of window
winW = 255
winH = 255
-- Range of coordinates (selected according to range of values the mandelbrot set takes on)
topLeft     = ((-2.0), (-2.0))
bottomRight = ((2.0), (2.0))

-- pixelToPoint/lookup2 function
-- For each dimension(x,y), find how far the currently selected screen pixel is
-- across the screen as a percentage... e.g (x / width) -> distance as percentage

-- Using the percentage, find it in terms of normalised coordinates by
-- multiplying by the range of the coordinate plane... e.g (x / width) * (maxX - minX) -> distance in coordinate space

-- Find the normalised coordinate position by adding the distance to the beginning
-- of the plane... e.g minX + (x / width) * (maxX - minX) -> corresponding coordinate
pixelToPoint :: (Int, Int) -> Point
pixelToPoint (x, y) = ((minX + ((xd / wd) * (maxX - minX))), (maxY - ((yd / hd) * (maxY - minY))))
        where minX = getX topLeft
              minY = getY topLeft
              maxX = getX bottomRight
              maxY = getY bottomRight
              xd = fromIntegral x
              yd = fromIntegral y
              wd = fromIntegral winW
              hd = fromIntegral winH

pointColour :: Point -> Pixel8
pointColour p = case approxTest 100 p of
  True -> 210
  False -> 0

type Point = (Float, Float)

getX (x, _) = x
getY (_, y) = y

next :: Point -> Point -> Point
next (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)

mandelbrot :: Point -> [Point]
mandelbrot p = iterate (next p) (0, 0)

fairlyClose :: Point -> Bool
fairlyClose (u, v) = (u * u + v * v) < 100

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))
