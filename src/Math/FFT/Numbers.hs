module Math.FFT.Numbers where    
import Data.Complex
    
one :: Num a => a     
one  = fromInteger 1

zero :: Num a => a
zero = fromInteger 0

i :: Num a => Complex a
i   = zero :+ one

two :: Num a => a
two = one + one  

e :: Floating a => a 
e = exp one