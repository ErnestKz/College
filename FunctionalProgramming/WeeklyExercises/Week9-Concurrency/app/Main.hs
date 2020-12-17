--   Week 9 - Concurrency
--   Ernests Kuznecovs -- 17332791

import Control.Concurrent
import Control.Monad
import System.IO

import Data.Char

main :: IO ()
main = do
  computations <- newChan
  putStr "input range from 5,000 to 10,000 gives good computation time for demo\n"
  inputProcessingLoop computations

inputProcessingLoop :: Chan ChanItem -> IO ()
inputProcessingLoop c = do

  putStr ">>= "
  hFlush stdout
  input <- getLine
  case parseCommand input of 
    Result      -> do
      print "checking if anything's ready"
      alarm_id <- forkIO $ knocker_upper c
      lookInChannel alarm_id
      inputProcessingLoop c
      
        where
          lookInChannel alarm_id = do
            item <- readChan c
            handleResult item
              where handleResult (Computation n) = print $ show n
                    handleResult (Alarm id)
                      | id == alarm_id   = print "nothing here"
                      | otherwise        = lookInChannel alarm_id
                      
    Compute n   -> do
      forkIO (computer c n)
      print ("added " ++ show n ++ " to computation channel")
      inputProcessingLoop c
    Quit        -> do
      alarm_id <- forkIO $ knocker_upper c
      print ("checking if anything's left over")
      flushChannel alarm_id
      return ()
      
      where
        flushChannel alarm_id = do
          item <- readChan c
          handleResult item
            where
              handleResult (Computation n) = do
                print $ show n
                flushChannel alarm_id
              handleResult (Alarm id)
                | id == alarm_id = print "ending program"
                | otherwise = flushChannel alarm_id
                
    Unknown     -> do
      print "unrecognised command"
      inputProcessingLoop c

parseCommand :: String -> Command
parseCommand s
  | s == "result"        = Result
  | length s >= 1 &&
    and (map isNumber s) = Compute $ read s
  | s == "quit"          = Quit
  | otherwise            = Unknown

data Command = Unknown
             | Result
             | Compute Int
             | Quit

data ChanItem = Computation Integer
              | Alarm ThreadId


computer :: Chan ChanItem -> Int -> IO ()
-- seems like the channel blocks putting other things in when this is happening
computer c n = writeChan c $ Computation $ primes !! n

knocker_upper :: Chan ChanItem -> IO ()
knocker_upper c =
  do
  threadDelay 1000000 
  id <- myThreadId
  writeChan c (Alarm id)

primes = sieve [2..]
sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p > 0]
-- https://stackoverflow.com/questions/63596587/generating-finite-lists-of-primes-in-haskell



