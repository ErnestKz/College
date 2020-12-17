{-# Language MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

import Control.Monad

-- Class of things that can transform monads into other monads.
-- if "m" is some arbitrary monad, then some other monad "t m"
-- can wrap around it so long as we provide a way convert any
-- m-action into a (t m)-action.
-- Notice the extra type parameter in "t" -- it's not a monad on it's
-- own, it's a thing that transforms one monad into another!
-- So "t m" is a monad (if "m" is), but "t" on it's own is some other thing.
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

-- Class for writer-compatible monads
--    Some monad "m" can be a writer if it implements this:
--    ("w" is the type of things we gather up. It has to be a moniod so that
--     we can make use of 'mappend :: m -> m -> m" to join values as they
--     are written. In the simple case "m" might be the list monoid, for
--     example, in which case 'mappend = (++)' ).
class (Monoid w, Monad m) => MonadWriter w m | m -> w where
  tell :: w -> m ()


-- OK, now here we go...

-- This is one example of something that can convert monads to writer monads:
newtype WriterT w m a = WriterT { runWriterT :: m (a,w) }

-- Now, I need two things:
--   1. I need (WriterT w m) to be a monad
--      (given that w is some "log" type, and "m" is some monad)
--   2. I also need WriterT to be a MonadWriter

-- For part 1:

-- liftM is fine here because we promise "m" will be a monad
instance (Monad m, Monoid w) => Functor (WriterT w m) where
  fmap = liftM

-- make it an applicative by using the "return" from the monad instance
-- of "m", and an empty writer log
instance (Monad m, Monoid w) => Applicative (WriterT w m) where
  pure a = WriterT $ return (a, mempty)
  (<*>) = ap

-- Join two writer log actions together by doing both things and
-- then concatenating the logs
instance (Monad m, Monoid w) => Monad (WriterT w m) where
    m >>= g = WriterT $ do
              (a, w1) <- runWriterT m
              (b, w2) <- runWriterT (g a)
              return (b, w1 `mappend` w2)
-- For part 2:

instance (Monoid w, Monad m) => MonadWriter w (WriterT w m) where
  tell w = WriterT $ return ( (), w )


-- A minimal example of a transformed monad:

-- 1. Introduce the identity monad:

newtype I a = I a

unwrapI (I a) = a

instance Functor I where
  fmap = liftM

instance Applicative I where
  pure = I
  (<*>) = ap

instance Monad I where
  (I a) >>= f = f a

-- Now make a writer monad that transforms the identity:

type Writer w = WriterT w I

-- And we're off:

test :: I ( (), String )
test = runWriterT $ do
  tell "foo "
  tell "bar "
  tell "baz"

main :: IO ()
main = print $ unwrapI test
