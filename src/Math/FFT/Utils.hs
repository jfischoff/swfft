module Math.FFT.Utils where
    
isPowerOfTwo :: Int -> Bool
isPowerOfTwo n
	| 0	<- n		 = True
	| 2	<- n		 = True
	| n `mod` 2 == 0 = isPowerOfTwo (n `div` 2)
	| otherwise		 = False