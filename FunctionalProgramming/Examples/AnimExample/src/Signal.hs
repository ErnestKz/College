-- A very simple library for manipulating continuous signals.
--

module Signal(Time
              , Signal
              , at
              , timeS
              , test1) where
import Control.Monad -- (forM_)


-- Instances

instance Functor Signal where
  fmap f xs = pure f <*> xs

instance Applicative Signal where
  pure x = Signal $ const x
  fs <*> xs = Signal $ \t -> (fs `at` t)  (xs `at` t)

-- Time varying signal
timeS  ::      Signal Time
timeS = Signal id

-- Transforming the time.
mapT   :: (Time -> Time)  -> Signal a -> Signal a
mapT f xs = Signal (at xs . f)

type Time = Double
newtype Signal a = Signal {at :: Time -> a}

--------------------------------------------
-- Example

-- sinusoidal of given frequency
--sinS :: Double -> Signal Double
--sinS freq = mapT (freq*) $ fmap sin timeS

-- sinusoidal signal
sinS :: Signal Double
sinS = fmap sin timeS


scale :: Num a =>  Signal a -> Signal a
scale = fmap ((30*) . (1+))

-- Discretize a signal
discretize :: Signal Double -> Signal Int
discretize = fmap round

-- convert to "analog"
toBars :: Signal Int -> Signal String
toBars = fmap (`replicate` '#') 

displayLength = 500
-- display the signal at a number of points
display :: Signal String -> IO ()
display ss = forM_ [0, 0.1 .. displayLength] $ \x ->
   putStrLn (ss `at` x)

-- The display magic.
-- Note how we take advantage of function composition, 
-- types defined so far, etc.
magic :: Signal Double -> IO ()
magic = display . toBars . discretize . scale

test1' :: IO ()
test1' = magic $ sinS -- 0.1

test1 = display . toBars . discretize . scale $ sinS
