module Math.FFT.Utils where
import Math.FFT.Numbers
import Data.Complex
    
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
	| 0	<- n		 = True
	| 2	<- n		 = True
	| n `mod` 2 == 0 = isPowerOfTwo (n `div` 2)
	| otherwise		 = False

rootsOfUnity :: (Floating a, Num a, RealFloat a) => Complex a -> Complex a -> Complex a
rootsOfUnity n index = e ** (negate one * two * pi * i * index * recip n)