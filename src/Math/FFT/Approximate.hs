{-# LANGUAGE FlexibleInstances #-}
module Math.FFT.Approximate where
import Data.Complex    
    
class Approximate a where
    (=~=) :: a -> a -> Bool
    
instance (Approximate a) => Approximate [a] where
    xs =~= ys = all (uncurry (=~=)) $ zip xs ys
    
instance Approximate Double where
    x =~= y = abs (x - y) < 0.001
    
instance Approximate (Complex Double) where
    (x :+ y) =~= (x' :+ y') = (x =~= x') && (y =~= y')
    