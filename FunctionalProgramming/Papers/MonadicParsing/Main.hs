newtype Parser a = Parser { parse :: (String -> [(a,String)]) }

item :: Parser Char
item =  Parser (\cs -> case cs of
                         ""     -> []
                         (c:cs) -> [(c,cs)])

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser (\cs -> case parse p cs of
                              []         -> []
                              [(x, cs')] -> [(f x, cs')])

-- with fmap we can create a new parser from an existing parser, with a function
-- applied to the parser's output.
  -- parse (fmap toUpper item) "outkast"
  
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser (\cs -> [(a, cs)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser (\cs -> case parse p1 cs of
                               []       -> []
                               [(x,cs')]-> parse (fmap x p2) cs)
              
  -- parse ((pure toUpper) <*> item) "foo"
           
instance Monad Parser where
  return a = Parser (\cs -> [(a, cs)])
  p >>= f  = Parser (\cs -> concat [parse (f a) cs'
                                   | (a, cs') <- parse p cs])












