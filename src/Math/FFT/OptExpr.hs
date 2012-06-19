module Math.FFT.OptExpr where
import Math.FFT.Utils
import Math.FFT.Numbers
import Data.Complex
import Data.Default
import Prelude hiding ((.))
import Control.Category
import Debug.Traced

-- All expression are prefect trees    
data Expr a = Add a a (Expr a) (Expr a)
            | Sub a a (Expr a) (Expr a)
            | Value a
            deriving (Show, Eq)
            
getCache (Add _ c _ _) = c
getCache (Sub _ c _ _) = c
getCache (Value c    ) = c

-- | update leaves the right branch totally intact, recursively. 
--   It is basically rotating the branches recursively on the left branch
--   and updating what used to be the left most bottom node.
--   I would like to not recaluate the unmodified right branches
update :: Num a => a -> Expr a -> Expr a
update v e = go e where
    go (Add u c x y) = cache $ Add u c y newX where newX = go x 
    go (Sub u c x y) = cache $ Sub u c y newX where newX = go x 
    go (Value _)     = Value v      
        
cache :: Num a => Expr a -> Expr a
cache (Add u _ x y) = Add u (eval x + u * eval y) x y
cache (Sub u _ x y) = Sub u (eval x - u * eval y) x y
cache x             = x
        
-- | Nothing special here. 
eval :: Num a => Expr a -> a
eval = getCache

-- | This is what I would like to optimize. 
-- | I should not have to reevaluate the whole thing, 
-- | but only what will be different based on the update.
evalList :: (Num a) => Expr a -> [a] -> [a]
evalList e xs = reverse $ go e xs [] where
    go e' []     outs = outs
    go e' (y:ys) outs = go (update y e') ys ((eval e'):outs)

twoPower 1 = 1
twoPower n = 1 + twoPower (n `div` 2)

-- | TODO. get this to compile. actually write the need functions
--   quick check agains the reference
--   make setup the list version and make sure it is the same
--   use criterion to compare
buildTree :: (Num a) => Int -> [Complex a] -> Expr (Complex a)
buildTree index xs = go [] (twoPower $ length xs) where
    go path 0     = Value $ lookupAt path xs
    go path level = (pathToExpr index path) (makeUnity index path) one 
                        (go ((False):path) (level - 1)) 
                        (go ((True ):path) (level - 1))

buildManyTrees :: (Num a) => [Complex a] -> [Expr (Complex a)]
buildManyTrees = undefined

--The main idea is that maybe there 

fft = map (eval . cache) . buildManyTrees

pathToExpr  path = undefined
pathToIndex index path = undefined 
lookupAt path xs       = undefined

makeUnity index path = e ** (negate one * two * pi * i * recip n) where
    i = pathToIndex index path
    n     = 2 ** (fromIntegral $ length path)

testExpr0 :: Expr Int
testExpr0 = cache $ Add 1 3 (Value 1) (Value 2)

testExpr1 :: Expr Int
testExpr1 = cache $ Add 1 7 (Value 3) (Value 4)

testExpr2 :: Expr Int
testExpr2 = cache $ Add 2 10 testExpr0 testExpr1

test0 :: [Int]
test0 = evalList testExpr2 [4, 5, 6] -- [3, 6, 9]

test1 :: [Int]
test1 = evalList testExpr2 [4, 5, 6] -- [10, 13, 15]