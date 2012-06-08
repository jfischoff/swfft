module Math.FFT.SlidingWindow where
import Debug.Traced
import Data.Complex
import Control.Arrow
import Debug.Trace
import Data.Complex
import Math.FFT.Numbers
  
dft :: (Floating a, Num a, RealFloat a, Fractional a) => [Complex a] -> [Complex a]  
dft = dft' True

dft' b xs = [convert b xs k | k <- [0 .. (length xs) - 1]]

invDft :: (Floating a, Num a, RealFloat a, Fractional a) => [Complex a] -> [Complex a]  
invDft xs = map (/ (fromIntegral $ length xs)) $ dft' False xs

convert :: (Floating a, Num a, RealFloat a) => Bool -> [Complex a] -> Int -> Complex a
convert b xs k = sum $ map (\(value, index) -> gconvert b (fromIntegral $ length xs) 
	(fromIntegral k) (fromIntegral index) value) $ zip xs [0..]

gconvert :: (Floating a, Num a, RealFloat a) => Bool -> Complex a -> Complex a -> Complex a -> Complex a 
        -> Complex a
gconvert True  l k n x_n = x_n * (e ** ((negate one) * two * pi * i * k * n * (recip l)))
gconvert False l k n x_n = x_n * (e ** (one * two * pi * i * k * n * (recip l)))

isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
	| 0	<- n		= True
	| 2	<- n		= True
	| n `mod` 2 == 0	= isPowerOfTwo (n `div` 2)
	| otherwise		= False

fft xs | length xs == 1         = xs
fft xs | isPowerOfTwo $ length xs = decimate' xs
fft xs | otherwise = error "Something went wrong!"

decimate' xs = result where
	even_values = get_even xs
	odd_values  = get_odd xs
	result      = twiddle (fft even_values) (fft odd_values)
	
twiddle evens odds = result where
	n = length evens
	tops    = map (top evens odds)    [0..n - 1]
	bottoms = map (bottom evens odds) [n..(2 * n) - 1]
	result  = tops ++ bottoms	

top evens odds k = combine evens odds k one
bottom evens odds k = combine evens odds (k - (length evens)) (negate one)

combine evens odds index sign = result where
	n = length evens * 2
	evensElement = evens !! index
	oddsElement  = (odds !! index)
	result = evensElement + 
	    (sign * (roots_of_unity (fromIntegral n) (fromIntegral index)) * 
		    oddsElement)
	
roots_of_unity n index = result where
	result = e ** ((negate one) * (two * pi * i * index) / n)

get_odd  = get_blah odd
get_even = get_blah even
get_blah test xs = map fst $ filter (test . snd) $ zip xs [0..]
