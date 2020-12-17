-- Ernests Kuznecovs
-- 17332791

data List a = Nil | Cons a (List a)
  deriving Show

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure a = Cons a Nil

instance Monad List where
  Nil >>= _ = Nil
  m   >>= f = listConcat (fmap f m)
    where listConcat Nil = Nil
          listConcat (Cons x xs) = listCombine x (listConcat xs)
            where listCombine (Cons x xs) ys = (Cons x (listCombine xs ys))
                  listCombine Nil ys = ys


-- Left identity:     return a >>= f   ==   f a

-- f :: a -> (List b)

-- LHS
-- The defined bind will simply pass the single element list i.e Cons a Nil
-- to fmap where f will be applied, this will create a nested list :: List (List b) but bind also
-- unnests the result via listConcat :: List (List a) -> List a, resulting into :: List b.
-- This is the same as the RHS.

-- RHS
-- f a :: List b


-- Right identity:    m >>= return     ==    m
-- Here, fmap return is applied to some  m :: List a, this will create :: List (List b), since
-- return :: a -> (List a) is applied to all items of the List m, but then bind unests the
-- result with listConcat, same as above. This will result with the original list, m.

-- Associativity:     (m >>= f) >>= g    ==    m >>= (\x -> f x >>= g)

-- On the RHS since \x -> f x can return any number of things since its of type :: List a, g gets mapped to all of the resulting
-- functors, in this way the fmaps are queued up to be applied.

-- The LHS plays into our intuition the most since we can see that f is first applied to m and then g is applied to the result.


data Pair a b = P a b
  deriving Show

instance Functor (Pair a) where
  fmap f (P a b) = P (a) (f b)

-- Can't create an instance of applicative for Pair since it can hold two different types
-- can't have some type as a default since all types would be equally wrong since it would break
-- the properties that valid applicatives can provide.

main = do
  let xs = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ Cons 5 $ Nil
  print (fmap (+1) xs)
  print (fmap (+1) (pure 1 :: List Int))
  print (xs >>= \x -> pure (1 + x))
  let f = \x -> Cons x (Cons x Nil)
  let g x = pure (x * 2)
  print ((xs >>= f) >>= g)
  print (xs >>= (\x -> f x >>= g))
  let myPair = P 'c' 2
  print myPair
  print (fmap (+1) myPair)
  
