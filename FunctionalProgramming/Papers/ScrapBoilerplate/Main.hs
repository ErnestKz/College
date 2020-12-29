{-# LANGUAGE Rank2Types #-}

-- forall allows us to use polymorphic functions
-- while also having the necessity of the argument
   -- of the polymorphic type to be of Typeclass b

-- i.e b can be of any time, as long as its a type class of data

-- gmap in these examples is encoding where the data stops
class Data a where
  gmapQ ::(forall b. Data b => (b -> r)) -> a -> [r]

instance Data a => Data [a] where
  gmapQ f []     = []
  gmapQ f (x:xs) = [f x, f xs]

instance Data Int where
  gmapQ f i = []

instance Data Char where
  gmapQ f c = []

instance Data Bool where
  gmapQ f c = []

gsize :: Data a => a -> Int
gsize t = 1 + sum (gmapQ gsize t)

gshow :: Data a => a -> String
gshow t = "(" ++ concat (intersperse " " (gmapQ gshow t)) ++ ")"
