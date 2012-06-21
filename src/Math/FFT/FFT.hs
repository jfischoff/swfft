module Math.FFT.FFT where
import Debug.Traced
import Data.Complex
import Control.Arrow
import Debug.Trace
import Data.Complex
import Math.FFT.Numbers
import Debug.Trace
import Debug.Trace.Utils
import Math.FFT.Utils

rlength :: (Floating a, Num a, RealFloat a, Show a) => [b] -> Complex a
rlength = fromIntegral . length

indices :: (Floating a, Num a, RealFloat a, Show a) => [Complex a]
indices = map fromIntegral [0..]


-- | Split the even and the odds. Call fft on each
--   Combine the results through twiddle
fft :: (Floating a, Num a, RealFloat a, Show a) => [Complex a] -> [Complex a]
fft xs | length xs == 1           = xs
fft xs | isPowerOfTwo $ length xs = (fft $ get_even xs) `twiddle` (fft $ get_odd xs)
fft xs | otherwise = error "Something went wrong!"


-- | for top half of the even and the odds
--   combine with a positive sign
--   for the bottom half combine with a negative sign
twiddle :: (Floating a, Num a, RealFloat a, Show a) => [Complex a] -> [Complex a] -> [Complex a]
twiddle evens odds =  result where
    n = rlength evens * two
    tops    = zipWith3 (combine one          n) evens odds indices  
    bottoms = zipWith3 (combine (negate one) n) evens odds indices
    result  = tops ++ bottoms	

-- | use the index to get a even and a odd element
--   combine the results but scale the odd by the roots of unity
combine :: (Floating a, Num a, RealFloat a, Show a) => Complex a -> Complex a -> Complex a 
    -> Complex a -> Complex a -> Complex a
combine sign n ev od index = ev + sign * rootsOfUnity n index * od
	

	
get_odd  = get_blah odd
get_even = get_blah even
get_blah test xs = map fst $ filter (test . snd) $ zip xs [0..]






