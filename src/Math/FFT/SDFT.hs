module Math.FFT.SDFT where
--import Math.FFT.Utils
import Math.FFT.Numbers
import Data.Complex
import Data.List
    
-- like length but returns a float
rlength :: (RealFloat a) => [b] -> Complex a 
rlength = fromIntegral . length

-- the sequence 0 :+ 0, 1 :+ 0, 2 :+ 0
rIndices :: (RealFloat a) => [Complex a]
rIndices = map fromIntegral [0..]

-- calculate a root of unity
rootsOfUnity' :: (RealFloat a) => Complex a -> Complex a -> Complex a
rootsOfUnity' n index = e ** (two * pi * i * index * recip n)

-- return all roots of unity for a given n as an infinite list
rootsOfUnity :: (RealFloat a) => Complex a -> [Complex a]
rootsOfUnity n = map (rootsOfUnity' n) rIndices

-- Takes in a list of previous spectrum values and the old first value 
-- and the new end value return a new list
sdft :: (RealFloat a) => [Complex a] -> Complex a -> Complex a -> [Complex a]
sdft spectrum oldFirst newEnd = zipWith shift spectrum (rootsOfUnity n) where
    shift kThSpectrum root = (kThSpectrum - oldFirst + newEnd) * root
    n = rlength spectrum

slideWindow :: (RealFloat a) 
            => ([Complex a] -> [Complex a])
            -> ([Complex a] -> Complex a -> Complex a -> [Complex a]) 
            -> Int
            -> [Complex a] -> [[Complex a]]
slideWindow fft f count xs = result where
    result  = scanl (\i (o, n) -> f i o n) initial $ zip xs rest
    initial = fft $ take count xs
    rest    = drop count xs 

window :: Int -> [a] -> [[a]]
window count xs | count > length xs = []
window count xs | otherwise = take count xs : window count (tail xs)

    
    
    
    
    
    
    
    
    
    
    
