module Math.FFT.SlidingWindow where
import Debug.Traced
import Data.Complex
import Control.Arrow
import Data.Complex
import Math.FFT.Numbers

data Sin a = Sin
    {
        _sinAmp    :: a,
        _sinPhase  :: a,
        _sinPeriod :: a
    }
    
data Cos a = Cos
    {
        _cosAmp    :: a,
        _cosPhase  :: a,
        _cosPeriod :: a
    }
    
data Euler a = Euler 
    {
        _eulerAmp    :: a,
        _eulerPeriod :: a
    }
    
class Generator a where
    apply :: (Real a, Num a, Enum a, Fractional a, Floating a) 
          => a -> b -> b
    
instance (Real a, Num a, Enum a, Fractional a, Floating a) 
    => Generator (Sin a) where
          apply (Sin amp phase period) x = amp * sin (period * x + phase) 

instance (Real a, Num a, Enum a, Fractional a, Floating a) 
  => Generator (Cos a) where
        apply (Cos amp phase period) x = amp * cos (period * x + phase)
        
instance (Real a, Num a, Enum a, Fractional a, Floating a) 
    => Generator (Euler a) where
        apply (Euler amp period) x = amp * exp (period * x + phase)

instance (Real a, Num a, Enum a, Fractional a, Floating a)
    => Generator (  -> a)

---------------------------Functions------------------------------------
sine :: SinConfig -> a -> a
sine config x = amp * sin (period * x * delta)



----------------------------General-------------------------------------
listGen :: b -> (b -> a) -> [a]
listGen start f = map f $ enumFrom start

------------------------------ Sin -------------------------------------
sinList :: (Real a, Num a, Enum a, Fractional a, Floating a) 
        => a -> a -> a -> a -> a -> [a]    
sinList amp period phase delta a = listGen a sinGen where
    sinGen x = amp * sin (period * x * delta) 

--------------------------------e----------------------------------------
eList :: (Real a, Num a, Enum a, Fractional a, Floating a) 
    => a -> a -> a -> a
eList amp period delta a = listGen a eGen where
    gen x = amp * (exp (x * delta) * two * pi)

-----------------------------Samplers------------------------------------
sampleList :: (b -> [a]) -> b -> b -> b -> Int -> [a]
sampleList f delta a startTime count = take count $
    f (startTime + (recip (fromIntegral count))) a

sampleSin :: (Real a, Num a, Enum a, Fractional a, Floating a) 
          => a -> a -> a -> a -> a 
          -> a -> Int -> [a]    
sampleSin amp period phase delta a startTime count = take count $ 
    sinList amp period phase (startTime + (recip (fromIntegral count))) a
    
countOperations ::  Traceable a => TracedExp a -> Int
countOperations = length . words . show 

sampleNFreq :: Int -> Int -> a
sampleNFreq count n = result where
    result = sampleSin 1.0 period