module Main where
import System.Random
import Control.Monad.State

main = do answer <- getStdRandom (randomR (1,100)) -- think of a number
          putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
          guesses <- execStateT (guessSession answer) 0
          putStrLn $ "Success in " ++ show guesses ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
    do gs <- lift getLine    -- get guess from user
       let g = read gs       -- convert to number
       modify (+1)           -- increment number of guesses
       case compare g answer of
              LT -> do lift $ putStrLn "Too low"
                       guessSession answer
              GT -> do lift $ putStrLn "Too high"
                       guessSession answer
              EQ -> lift $ putStrLn "Got it!"



data Vars = Vars {
  var1 :: Int,
  var2 :: Float }

type MyState a = StateT Vars IO a
type Selector a = (MyState a, a -> MyState ())

s1 :: Selector Int
s1 = (gets var1, \x -> modify (\vs -> vs {var1 = x}))
