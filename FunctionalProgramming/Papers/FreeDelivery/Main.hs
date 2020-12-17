{-# LINE 19 "delivery.lhs" #-}
{-#  LANGUAGE StandaloneDeriving  #-}
{-#  LANGUAGE KindSignatures  #-}
{-#  LANGUAGE GADTs  #-}
{-#  LANGUAGE FlexibleContexts  #-}
{-#  LANGUAGE UndecidableInstances  #-}
{-#  LANGUAGE ExistentialQuantification  #-}
{-#  LANGUAGE DeriveFunctor  #-}

import Control.Concurrent (threadDelay)
import System.Random (Random(..), randomR, randomIO, randomRIO)

import Control.Monad (unless,forever)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
{-# LINE 187 "delivery.lhs" #-}
data Command :: * -> *   where
  Say     :: String  -> Command ()
  Toast   :: Int     -> Command ()
  Sense   :: ()      -> Command Integer
{-# LINE 211 "delivery.lhs" #-}
deriving instance Show (Command r)
{-# LINE 220 "delivery.lhs" #-}
data Name = NSay | NToast | NSense
  deriving Show
{-# LINE 225 "delivery.lhs" #-}
readCommand :: String -> (Name, String)
readCommand s = head $
  [ (NSay, s')    | ("Say", s')    <- lex s ] ++
  [ (NToast, s')  | ("Toast", s')  <- lex s ] ++
  [ (NSense, s')  | ("Sense", s')  <- lex s ]
{-# LINE 251 "delivery.lhs" #-}
readReply :: Command r -> String -> r
readReply (Say    _)  s  = read s
readReply (Toast  _)  s  = read s
readReply (Sense  _)  s  = read s
{-# LINE 268 "delivery.lhs" #-}
type ServerBehaviour = String -> IO String
{-# LINE 291 "delivery.lhs" #-}
mainServer :: IO ()
mainServer = server ts
{-# LINE 296 "delivery.lhs" #-}
mainClient :: String -> IO ()
mainClient s = do
  response <- client s
  putStrLn ("Received " ++ response)
{-# LINE 317 "delivery.lhs" #-}
ts :: String -> IO String
ts s = do { putStrLn s ; return s }
{-# LINE 328 "delivery.lhs" #-}
execSay :: String -> IO ()
execSay s = putStrLn s

execToast :: Int -> IO ()
execToast n = do  putStr ("Toasting...") 
                  threadDelay (1000000*n) 
                  putStrLn ("done!")

execSense :: IO Integer
execSense = randomRIO (0,100)
{-# LINE 344 "delivery.lhs" #-}
execCommand :: ServerBehaviour
execCommand s = case readCommand s of
  (NSay, s')    -> do { r <- execSay (read s') ; return (show r) }
  (NToast, s')  -> do { r <- execToast (read s') ; return (show r) }
  (NSense, s')  -> do { r <- execSense ; return (show r) }

commandServer :: IO ()
commandServer = server execCommand
{-# LINE 375 "delivery.lhs" #-}
commandClient :: Command r -> IO r
commandClient c =  do  
                     r <- client (show c) 
                     return (readReply c r)
{-# LINE 405 "delivery.lhs" #-}
data FreeM f a = Var a | Com (f (FreeM f a))
{-# LINE 409 "delivery.lhs" #-}
instance Functor f => Monad (FreeM f) where
  return = Var
  Var a     >>= k  = k a
  Com x     >>= k  = Com (fmap (>>= k) x)
{-# LINE 420 "delivery.lhs" #-}
deriving instance  (Show (f (FreeM f a)), Show a) => 
                   Show (FreeM f a) 
{-# LINE 432 "delivery.lhs" #-}
data Action a = forall r . Action (Command r, r->a)

instance Functor Action where
  fmap f (Action (c, k)) = Action (c, f . k)
{-# LINE 440 "delivery.lhs" #-}
type Program a = FreeM Action a
{-# LINE 446 "delivery.lhs" #-}
effect :: Command r -> Program r
effect c = do { Com (Action (c, Var)) }

say :: String -> Program ()
say s = effect (Say s)

toast :: Int -> Program ()
toast n = effect (Toast n)

sense :: Program Integer
sense = effect (Sense ())
{-# LINE 460 "delivery.lhs" #-}
straight :: Program ()
straight = do { say "hello" ; toast 3 ; say "goodbye" }
{-# LINE 465 "delivery.lhs" #-}
branch :: Program ()
branch = do { t <- sense ; if t<80 then toast 3 else say "hot" }
{-# LINE 476 "delivery.lhs" #-}
data ActionF a =  forall r . (Bounded r, Enum r) => 
                  ActionF (Command r, r->a)

instance Show a => Show (ActionF a) where
  show (ActionF (c,k)) = show c ++ " " ++ 
    show [show (k r) | r <- [minBound..maxBound]]

type ProgramF a = FreeM ActionF a
{-# LINE 494 "delivery.lhs" #-}
data Temperature = Low | Medium | High 
  deriving (Show, Read, Enum, Bounded)

data CommandF :: * -> *   where
  SayF    :: String  -> CommandF ()
  ToastF  :: Int     -> CommandF ()
  SenseF  :: ()      -> CommandF Temperature
{-# LINE 506 "delivery.lhs" #-}
readFirstFreeM :: String -> (Bool, String)
readFirstFreeM s = head $
  [ (True, s')   | ("Var", s')  <- lex s ] ++
  [ (False, s')  | ("Com", s')  <- lex s ] 
{-# LINE 520 "delivery.lhs" #-}
execProgram :: String -> IO String
execProgram s = case readFirstFreeM s of
  (True, s') -> return s'
  (False, s') -> case readCommandF s' of
    (NSayF, s'')    -> do  (m,s''') <- readOne s''
                           execSay m 
                           execProgram (head (read s''')) 
    (NToastF, s'')  -> do  (n,s''') <- readOne s''
                           execToast n 
                           execProgram (head (read s''')) 
    (NSenseF, s'')  -> do  ((),s''') <- readOne s''
                           t <- execSenseF
                           execProgram (read s''' !! fromEnum t) 
{-# LINE 536 "delivery.lhs" #-}
readOne :: Read a => String -> IO (a, String)
readOne s = return (head (reads s))
{-# LINE 551 "delivery.lhs" #-}
programServer :: IO ()
programServer = server execProgram
{-# LINE 556 "delivery.lhs" #-}
programClient :: (Show a, Read a) => ProgramF a -> IO a
programClient p = do { r <- client (show p) ; return (read r) }
{-# LINE 573 "delivery.lhs" #-}
instance Functor f => Functor (FreeM f) where
  fmap f (Var a)  = Var (f a)
  fmap f (Com x)  = Com (fmap (fmap f) x)

instance Functor f => Applicative (FreeM f) where
  pure = Var
  Var f <*> Var a  = Var (f a)
  Var f <*> Com y  = Com (fmap f <$> y)
  Com x <*> b      = Com ((<*> b) <$> x)

instance Random Temperature where
  random = randomR (Low,High)
  randomR (l,h) g = let (n,g') = randomR (fromEnum l,fromEnum h) g 
                    in (toEnum n,g')

execSenseF :: IO Temperature
execSenseF = randomIO

data NameF = NSayF | NToastF | NSenseF
  deriving Show

readCommandF :: String -> (NameF, String)
readCommandF s = head $
  [ (NSayF, s')    | ("Say", s')    <- lex s ] ++
  [ (NToastF, s')  | ("Toast", s')  <- lex s ] ++
  [ (NSenseF, s')  | ("Sense", s')  <- lex s ]
{-# LINE 643 "delivery.lhs" #-}
data FreeA :: (* -> *) -> * -> *   where
  Pure  :: a ->                      FreeA f a
  More  :: f (b->a) -> FreeA f b ->  FreeA f a
{-# LINE 671 "delivery.lhs" #-}
instance Functor f => Applicative (FreeA f) where
  pure = Pure
  Pure f    <*> y = fmap f y
  More h x  <*> y = More (fmap uncurry h) (pure (,) <*> x <*> y)
{-# LINE 680 "delivery.lhs" #-}
data ActionA a = forall r . Read r => ActionA (Command r, r->a)
{-# LINE 684 "delivery.lhs" #-}
instance Functor ActionA where
  fmap f (ActionA (c, k)) = ActionA (c, f . k)
{-# LINE 689 "delivery.lhs" #-}
type ProgramA a = FreeA ActionA a
{-# LINE 710 "delivery.lhs" #-}
effectA :: Read r => Command r -> ProgramA r
effectA c = More (ActionA (c, \ r () -> r)) (Pure ())

sayA :: String -> ProgramA ()
sayA s = effectA (Say s)

toastA :: Int -> ProgramA ()
toastA n = effectA (Toast n)

senseA :: ProgramA Integer
senseA = effectA (Sense ())
{-# LINE 741 "delivery.lhs" #-}
straightA :: ProgramA (Integer, Integer)
straightA = pure (\ t () t' -> (t,t')) <*> senseA <*> toastA 3 <*> senseA
{-# LINE 751 "delivery.lhs" #-}
serializeA :: ProgramA a -> [String]
serializeA (Pure _)                  = []
serializeA (More (ActionA (c,_)) p)  = show c : serializeA p
{-# LINE 767 "delivery.lhs" #-}
deserializeA :: ProgramA a -> [String] -> a
deserializeA (Pure a) []  = a
deserializeA (More (ActionA (c,k)) p) (s:ss) 
                          = k (readReply c s) (deserializeA p ss)
{-# LINE 784 "delivery.lhs" #-}
execStraight :: String -> IO String
execStraight s = do  let reqs = read s
                     resps <- sequence (map execCommand reqs)
                     return (show resps)
{-# LINE 791 "delivery.lhs" #-}
straightServer :: IO ()
straightServer = server execStraight

straightClient :: ProgramA a -> IO a
straightClient p = do  r <- client (show (serializeA p))
                       return (deserializeA p (read r))
{-# LINE 803 "delivery.lhs" #-}
instance Functor f => Functor (FreeA f) where
  fmap f (Pure a)    = Pure (f a)
  fmap f (More h x)  = More (fmap (f .) h) x
{-# LINE 967 "delivery.lhs" #-}
server :: ServerBehaviour -> IO ()
server f = withSocketsDo $ do
  (addr:_) <- getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just "3000")
  s <- socket (addrFamily addr) Stream defaultProtocol
  bindSocket s (addrAddress addr)
  listen s 1
  forever $ do
    (conn, _) <- accept s
    talk f conn
    sClose conn
{-# LINE 984 "delivery.lhs" #-}
talk :: (String -> IO String) -> Socket -> IO ()
talk f conn =
 do req <- recv conn 4096
    unless (S.null req) $ do
      resp <- f (C.unpack req)
      sendAll conn (C.pack resp)
      talk f conn
{-# LINE 994 "delivery.lhs" #-}
client :: String -> IO String
client request = withSocketsDo $ do
  (addr:_) <- getAddrInfo 
    Nothing (Just "127.0.0.1") (Just "3000")
  s <- socket (addrFamily addr) Stream defaultProtocol
  connect s (addrAddress addr)
  sendAll s (C.pack request)
  response <- recv s 4096
  sClose s
  return (C.unpack response)
