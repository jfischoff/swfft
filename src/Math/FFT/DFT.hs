module Math.FFT.DFT where
import Math.FFT.Numbers
import Data.Complex

dft :: (Floating a, Num a, RealFloat a, Fractional a, Show a) => [Complex a] -> [Complex a]  
dft = dft' True

dft' b xs = [convert b xs k | k <- [0 .. (length xs) - 1]]

invDft :: (Floating a, Num a, RealFloat a, Fractional a, Show a) => [Complex a] -> [Complex a]  
invDft xs = map (/ n) $ dft' False xs where
    n = fromIntegral $ length xs

convert :: (Floating a, Num a, RealFloat a, Show a) => Bool -> [Complex a] -> Int -> Complex a
convert b xs k = sum $ zipWith (gconvert b n k') xs $ map fromIntegral [0..(length xs - 1)] where
	    n  = fromIntegral $ length xs
	    k' = fromIntegral k

gconvert :: (Floating a, Num a, RealFloat a, Show a) => Bool -> Complex a -> Complex a -> Complex a 
    -> Complex a -> Complex a
gconvert True  l k x_n n = x_n * (e ** ((negate one) * two * pi * i * k * n * (recip l)))
gconvert False l k x_n n = x_n * (e ** (one * two * pi * i * k * n * (recip l)))

