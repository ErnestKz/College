module Shapes(
  Shape, Point, Vector, Transform, Drawing,
  point, getX, getY,
  empty, circle, square, polygon, ellipse, 
  identity, translate, rotate, scale, (<+>),
  inside)  where

import Data.Sort
import Codec.Picture
-- Utilities

data Vector = Vector Double Double
              deriving Show
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
           | Polygon [Point]
           | Ellipse Point Point Double 
             deriving Show

empty, circle, square :: Shape
polygon :: [Point] -> Shape
ellipse :: Point -> Point -> Double -> Shape

empty = Empty
circle = Circle
square = Square
ellipse f g c = Ellipse f g c
polygon points = Polygon points

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

-- Drawings

type Drawing = [(Transform,Shape,PixelRGB8, Integer)]

-- interpretation function for drawings

--mean :: [PixelRGB8] -> PixelRGB8

inside :: Point -> Drawing -> PixelRGB8
inside p d = colour
  --or $
  -- map (inside1 p) d
  where (colour, zindex) = head $ reverse $ sortOn (\(c, z) -> z) $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape, PixelRGB8, Integer) -> (PixelRGB8, Integer)
inside1 p (t,s,c,z) | insides (transform t p) s  = (c, z)
                    | otherwise                  = ((PixelRGB8 0 0 0), -1)

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
-- p `insides` Ellipse f = distance p <= (( 1 + (cos (crossVec p (Vector 0 1))) * f)) -- Lidl ellipse
p `insides` (Ellipse f g c) = (distanceBetween p f) + (distanceBetween p g) <= c
p `insides` Polygon ps = allOneSide EitherSide p ps
  where
    allOneSide _ _ (_:[]) = True
    allOneSide expectingSide p (p1:p2:ps) = 
      case expectingSide of
        EitherSide -> allOneSide side p (p2:(ps++[p1])) -- add starting point to the end
        otherwise  -> case (expectingSide == side) of
                        True  -> allOneSide side p (p2:ps)
                        False -> False
      where
        -- side = case ((angleAt p1 p p2) > pi) of
        --   True  -> LeftSide
        --   False -> RightSide
        vec0 =  normalise (translateToOrigin p1 p)
        vec1 =  normalise (translateToOrigin p1 p2)
        side = pointingDirection vec0 vec1

crossVec (Vector a b) (Vector a' b') = (a * b' - a' * b)

pointingDirection (Vector a b) (Vector a' b') | (a * b' - a' * b) < 0  = RightSide
                                              | otherwise              = LeftSide

data Side = LeftSide | RightSide | EitherSide
  deriving (Eq, Show)
        
distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

distanceBetween :: Point -> Point -> Double
distanceBetween (Vector x1 y1 ) (Vector x2 y2) = sqrt ( (x2 - x1)**2 + (y2 - y1)**2 )

angleAt :: Point -> Point -> Point -> Double
angleAt origin p1 p2 = acos( (a**2 + b**2 - c**2) / (2*a*b) )
  where
    a = (distanceBetween p1 origin)
    b = (distanceBetween p2 origin)
    c = (distanceBetween p1 p2)

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 20), circle)


-- first arugment is the vectors non (0,0) origin,
-- second argument is the vector
translateToOrigin :: Point -> Point -> Vector
translateToOrigin (Vector x y) (Vector x' y') = Vector (x' - x) (y' - y)

normalise :: Vector -> Vector
normalise v =
  (Vector a' b')
  where
    magnitude = distance v
    Vector a b = v
    a' = a / magnitude
    b' = b / magnitude





-- Closed polygon defined by a series of points
-- Need to represent a polygon such that when given
-- a point, can determine if its inside or not
-- using the insides function.

-- insides :: Point -> Shape -> Bool

-- A polygon will be a data type of Shape, and the
-- constructor will look like Polygon [Point]
-- Since the polygon will be defined by a series of points.

-- The vector of points must be a convex set for the implementation of insides to work.
-- The Polygon shape wil not check for this.

-- monad for changing where the orign is at?


  
