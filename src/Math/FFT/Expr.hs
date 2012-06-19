module Math.FFT.Expr where
import Prelude hiding ((.))
import Control.Category
import Debug.Traced

-- All expression are prefect trees    
data Expr a = Add   (Expr a) (Expr a)
            | Value a
            deriving (Show, Eq)

-- | update leaves the right branch totally intact, recursively. 
--   It is basically rotating the branches recursively on the left branch
--   and updating what used to be the left most bottom node.
--   I would like to not recaluate the unmodified right branches
update :: a -> Expr a -> Expr a
update v e = go e where
    go (Add x y) = Add y (go x) -- I flip and process on the old left branch
    go (Value _) = Value v      -- I've updated what used to be the left most node, 
                                -- and it is now the right most with a new value v
        
-- | Nothing special here. 
eval :: Num a => Expr a -> a
eval (Add x y) = (eval x) + (eval y)
eval (Value a) = a

-- | This is what I would like to optimize. 
-- | I should not have to reevaluate the whole thing, 
-- | but only what will be different based on the update.
evalList :: (Num a) => Expr a -> [a] -> [a]
evalList e xs = reverse $ go e xs [] where
    go e' []     outs = outs
    go e' (y:ys) outs = go (update y e') ys ((eval e'):outs)

testExpr0 :: Expr Int
testExpr0 = Add (Value 1) (Value 2)

testExpr1 :: Expr Int
testExpr1 = Add (Value 3) (Value 4)

testExpr2 :: Expr Int
testExpr2 = Add testExpr0 testExpr1

test0 :: [Int]
test0 = evalList testExpr2 [4, 5, 6] -- [3, 6, 9]

test1 :: [Int]
test1 = evalList testExpr2 [4, 5, 6] -- [10, 13, 15]