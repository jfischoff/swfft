{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
module Math.FFT.SlidingWindow where
import Debug.Traced
import Data.Complex
import Control.Arrow
import Debug.Trace
import Data.Complex
import Math.FFT.Numbers
import Debug.Trace
import Debug.Trace.Utils  
import Control.Monad.State
import Data.Data
import Data.Typeable
import Control.Applicative ((<$>), (<*>)) 
import Data.Lens.Common
import Data.Lens.Template
import GHC.Generics
  
data Level a = Level {
        levelEvens   :: [Level a],
        levelOdds    :: [Level a],
        rootsOfUnity :: [Complex a]
    } | Leaf Int 
    
data MutableState a = MutableState {
        _values    :: [Complex a],
        _start     :: Int
    }
    deriving(Show, Eq, Read, Typeable, Data)
    
type Context a b = State (MutableState a) b 

makeLenses [''MutableState]
    
setup :: [Complex a] -> Level a
setup = undefined

append :: Complex a -> Context a ()
append element = modify (append' element)    

append' :: Complex a -> MutableState a -> MutableState a
append' element (MutableState xs offset) = MutableState newValues (offset + 1) where
    newValues = insert element (offset `mod` length xs ) xs
    
insert :: a -> Int -> [a] -> [a]
insert element index xs = uncurry (++) $ second ((element:) . tail) $ splitAt index xs

eval :: Level a -> Context a [Complex a]
eval (Level evens odds roots) = do
    plused <- zipWith3 (combine True ) roots <$> mapM eval evens <*> mapM eval odds
    subbed <- zipWith3 (combine False) roots <$> mapM eval evens <*> mapM eval odds
    return $ plused ++ subbed
eval (Leaf x) = (:[]) <$> lookupValue x

lookupValue :: Int -> Context a (Complex a)
lookupValue index = do
    xs     <- gets _values
    start' <- gets _start
    let actualIndex = (start + index) `mod` length xs
    return $ xs !! actualIndex

combine :: Bool -> Complex a -> Complex a -> Complex a -> Complex a
combine True  root od ev = ev + root od
combine False root od ev = ev - root od


    