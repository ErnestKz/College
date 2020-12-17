import Control.Concurrent
import Control.Monad
import System.IO



main = do
  m <- newEmptyMVar
  forkIO (do putMVar m 'a'; putMVar m 'b')
  c <- takeMVar m
  print c
  c <- takeMVar m
  print c

main4 = do
  hSetBuffering stdout NoBuffering
  c <- newChan
  forkIO (worker c)
  forkIO (forever $ putChar '*')
  readChan c


worker :: Chan Bool -> IO ()
worker c = do
  mapM putChar "Printing all chars"
  writeChan c True


main3 = do
  hSetBuffering stdout NoBuffering
  forkIO (forever $ putChar 'o')
  forkIO (forever $ putChar 'O')
  threadDelay $ 10^6

main2 = do
  hSetBuffering stdout NoBuffering
  forkIO $ forever $ putChar 'o'
  replicateM_ 10000 $ putChar 'O'


main1 = do
  hSetBuffering stdout NoBuffering
  forkIO (forever $ putChar 'o')
  forkIO (forever $ putChar 'O')
