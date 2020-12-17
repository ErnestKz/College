-- Ernests Kuznecovs 
-- 17332791

data Writer a = Writer ([String], a)
  deriving Show
              
instance Functor Writer where
  fmap f (Writer (log, x)) = (Writer (log, f x))

instance Applicative Writer where
  pure x = Writer ([], x)
  (Writer (log1, fx)) <*> (Writer (log2, x)) = Writer (log1 ++ log2, fx x)

instance Monad Writer where
  return = pure
  (Writer (log1, x)) >>= f =
    let Writer (log2, y) = f x
    in Writer (log1 ++ log2, y)

tell :: String -> Writer ()
tell log = (Writer ([log], ()))

example :: Writer Int
example = do
  tell "entry 1"
  tell "entry 2"
  return (1 + 1)


-- The problem with the arbitrary type for the log is that
-- we assume that two logs are (++)able.

-- Need to create a new type class for
-- the personalised accumulated log that the user might want to implement.

-- In order to create an instance for this accumulated log, the user
-- would need to implement the operator that takes two logs and combines
-- them together into one.

-- ( didn't leave enough time to figure out the syntax to implement it )

-- data SupaWriter a = SupaWriter (Gluable b, a)
--   deriving Show
              
-- instance Functor SupaWriter where
--   fmap f (SupaWriter (log, x)) = (SupaWriter (log, f x))

-- instance Applicative SupaWriter where
--   pure x = SupaWriter ([], x)
--   (SupaWriter (log1, fx)) <*> (SupaWriter (log2, x)) = SupaWriter (log1 <+> log2, fx x)

-- instance Monad Writer where
--   return = pure
--   (Writer (log1, x)) >>= f =
--     let Writer (log2, y) = f x
--     in Writer (log1 <+> log2, y)

-- class Gluable a where
--   (<+>) :: a -> a -> a

-- instance Gluable [] where
--   (<+>) x y = x ++ y
