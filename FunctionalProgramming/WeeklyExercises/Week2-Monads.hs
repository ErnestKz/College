import Data.Char 

f1 :: [IO a] -> IO a
f1 ( x : [] ) = x >>= (\a -> return a)
f1 ( x : xs ) = x >> f1 xs

ioList = [putChar 'h', putChar 'e', putChar 'l', putChar 'l', putChar 'o']

main = f1 ioList
main2 = do
  let a = f1 actions
      b = if True then putChar 'a' else putChar 'b'
  a
  putStr "Hi there\n"
  where actions = ioList

-- Since haskell is lazy, it doesn't evaluate a and b unless they are used.

-- If you insert the a inside the do block, it will become a part of the IO chain,
-- it wil evaluate 'f1 actions', which causes a chain of IO actions and will
-- continue until it computes the new world on the rhs of f1 :: [IO a] -> IO a,
-- where the new world will be passed to the next IO action, in this case
-- putStr "Hi there\n"

-- The things inside the let block are just definitions haskell can refer
-- to in the case it needs the value a or b.

while :: IO Bool -> IO ()
while tbd = tbd >>= \ans -> if ans
                            then while tbd
                            else return ()
                                 
-- while is evaluating the IO action and then using the answer to decide what to do.

-- Since while tbd is of type IO (), haskell knows it will eventually return
-- IO (), but on the way, it may do any number of things to the outside world.

-- return () ties things up as it is of type IO a.

q :: IO Bool
q = putStrLn "Do you want to keep going?" >>
    getLine >>= \line -> case map toLower line of
                           "yes" -> return True
                           _     -> putStrLn "I'll take that as a no." >> return False


f2 :: [IO a] -> IO [a]
f2 [] = return []
f2 (x:xs) = x >>= \y -> f2 xs >>= \ys -> return (y:ys)

-- We get access to the resulting 'a' in an IO a when we are defining
-- the bind.

-- Then in combination with another bind with a recursive call to itself, it lets us recursively
-- construct the end result.

read10 :: IO String
read10 = f2 $ take 10 actions
  where actions = getChar : actions
                              
