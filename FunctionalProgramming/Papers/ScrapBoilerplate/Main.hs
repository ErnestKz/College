{-# LANGUAGE Rank2Types #-}

class Data a where
  gmapQ ::(forall b. Data b => (b -> r)) -> a -> [r]

instance Data a => Data [a] where
  gmapQ f []     = []
  gmapQ f (x:xs) = [f x, f xs]

instance Data Int where
  gmapQ f i = []


gsize :: Data a => a -> Int
gsize t = 1 + sum (gmapQ gsize t)
