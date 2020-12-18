module Render(Window,defaultWindow,samples,render) where
import Codec.Picture
import Shapes


--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered, 
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 50x50 pixel image
defaultWindow :: Window
defaultWindow = Window (point (-1.5) (-1.5)) (point 1.5 1.5) (50,50)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / (fromIntegral $ n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ] 

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

-- render a drawing into an image, then save into a file
-- NB: the lookup1 function is a VERY inefficient way to convert screen coordinates to drawing
--     coordinates! It should be possible to do this in O(1) time, not O(N) time!!
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window topLeft bottomRight (w,h) = win

      pixRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (colorForImage $ lookup2 (x,y))
--      pixRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) (colorForImage $ lookup1 (x,y) locations)
                              
      lookup1 :: (Int,Int) -> [((Int,Int), Point)] -> Point
      lookup1 a m = case lookup a m of
                      Nothing -> point 0 0
                      Just x  -> x

      -- pixelToPoint/lookup2 function
-- For each dimension(x,y), find how far the currently selected screen pixel is
-- across the screen as a percentage... e.g (x / width) -> distance as percentage

-- Using the percentage, find it in terms of normalised coordinates by
-- multiplying by the range of the coordinate plane... e.g (x / width) * (maxX - minX) -> distance in coordinate space

-- Find the normalised coordinate position by adding the distance to the beginning
-- of the plane... e.g minX + (x / width) * (maxX - minX) -> corresponding coordinate
      lookup2 :: (Int, Int) -> Point
      lookup2 (x, y) = point (minX + ((xd / wd) * (maxX - minX))) (maxY - ((yd / hd) * (maxY - minY)))
        where minX = getX topLeft
              minY = getY topLeft
              maxX = getX bottomRight
              maxY = getY bottomRight
              xd = fromIntegral x
              yd = fromIntegral y
              wd = fromIntegral w
              hd = fromIntegral h

      locations :: [ ((Int,Int), Point) ]
      locations = concat $ zipWith zip (coords win) (pixels win)
      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0
