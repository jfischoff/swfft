{-# LANGUAGE Rank2Types, TupleSections, DeriveFunctor #-}
module Math.FFT.Perfect where
import Prelude hiding ((.))
import Control.Category
-- All expression are prefect trees    
--data Perfect a = PZero a | PSucc (a, a)

             
data Base perfect a = Zero a | Succ (perfect (a, a))

newtype Perfect a = In (Base Perfect a)

fmap' :: (forall a. t a -> u a) -> Base t a -> Base u a
fmap' f (Zero a) = Zero a
fmap' f (Succ b) = Succ (f b)

wrap :: a -> [a]
wrap    a  = [a]

unwrap :: [a] -> a
unwrap    [a] = a

out (In a) = a

diverg :: (a -> Base perfect b) 
       -> (perfect (a, a) -> Base perfect b) 
       -> Base perfect a -> Base perfect b
diverg :: (a -> t b) -> (t (a, a) -> t b) -> t a
diverg f g (Zero a) = f a
diverg f g (Succ t) = g t
--
base :: (forall a. t a -> u a) -> Base t b -> Base u b
base f = diverg Zero Succ . fmap' f

banana :: (forall a. Base t a -> t a) -> Perfect b -> t b
banana f = f . base (banana f) . out 

lens :: (forall a. t a -> Base t a) -> t b -> Perfect b
lens f = In . base (lens f) . f      

if' :: (a -> Bool) -> (a -> b) -> (a -> b) -> a -> b
if' p f g x = if p x then f x else g x  

cata :: (a -> Base perfect b) 
     -> (perfect (a, a) -> Base perfect b)
     -> Perfect b -> perfect b
cata f g = banana (diverg f g)

flatten :: Perfect a -> [a]
flatten = undefined
          
--update :: a -> Expr a -> Expr a
--update v e = go e where
--    go (Add (x, y)) = Add (y, (go x))
    --go (Value _)    = Value v
    
--eval :: Num a => Expr a -> a
--eval (Add (x, y)) = (eval x) + (eval y)
--eval (Value a)    = a