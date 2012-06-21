{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Debug.Trace.Helpers
import Debug.Trace
import Test.QuickCheck.Checkers
import Data.List
import Data.Generics.Uniplate.Data
import Control.Applicative ((<$>))    
--import Math.FFT.SlidingWindow
import qualified Math.FFT.OptExpr as O
import Debug.Traced
import Data.Complex hiding (phase)
import qualified Numeric.FFT as N
import Math.FFT.Approximate
import Math.FFT.Numbers
import Math.FFT.FFT
import Math.FFT.DFT
import Math.FFT.SDFT
import Debug.TracedInternal

amp       = 1.0
period    = 1.0
phase     = 1.0
delta     = 1.0 
startTime = 1.0
count     = 4


main = defaultMain tests
--main = do
--    print "dft"
--    countAll dft
--    print "fft"
--    countAll fft
    
--f :: (Floating a, Num a, RealFloat a, Show a) => Complex a -> 
--    Complex a -> Complex a -> Complex a -> Complex a    
f :: (Floating a, Num a, RealFloat a, Show a) =>  a -> 
     a ->  a ->  a ->  a
--f x_n k n l = x_n * (e ** ((negate one) * two * pi * i * k * n * (recip l)))    
f x_n k n l = two * pi 
    
--main = do 
--    print "dft" 
--    print_ops dft
--    print "fft"
--    print_ops fft
--    print "gconvert"
--    putStr $ showAsExp $ 
--        f (mkTraced 1.0) (mkTraced 1.0) (mkTraced 1.0) (mkTraced 1.0)
            
countAll f = mapM_ (print . traceAndCount . f) [
                                        initial_test0,
                                        initial_test,
                                        initial_test_1,
                                        initial_test_2,
                                        initial_test_3, 
                                        initial_test_4]

countDFT = traceAndCount . dft
countFFT = traceAndCount . fft
    
traceAndCount = count_ops . (\(a :+ b) -> tracedD a) . head
    
print_ops f = putStr $ unwords $ map (\(a :+ b) -> showAsExp a ++ " :+ " ++ showAsExp b) $ 
    f $ (:[]) $ mkComplexTraced 1.0 


mkTraced :: Double -> Traced Double
mkTraced = traced


mkComplexTraced :: Double -> Complex (Traced Double)
mkComplexTraced = (\(a :+ b) -> traced a :+ traced b) . (:+ 2.0)


initial_test0 = map mkComplexTraced initial_test0'
initial_test0' = [1.0] :: [Double]


initial_test = map mkComplexTraced initial_test'
initial_test' = [1.0, 2.0] :: [Double]

initial_test_1 = map mkComplexTraced initial_test_1'
initial_test_1' = [1.0, 2.0, 3.0, 4.0] :: [Double]

initial_test_2 = map mkComplexTraced initial_test_2'
initial_test_2' = [1.0, 2.0, 3.0, 4.0, 2.0, 3.0, 4.0, 1.0] :: [Double]

initial_test_3 = initial_test_2 ++ initial_test_2

initial_test_4 = initial_test_3 ++ initial_test_3

count_ops (Name _ _ t)     = count_ops t
count_ops (Con _)          = 0
count_ops (Apply _ _ _ ts) = (+1) $ sum $ map (count_ops) ts
count_ops (Let _ t)        = count_ops t 

tests = [
            testGroup "dft" [
                --testProperty "dft equals N.dft" prop_dft_equals_n_dft,
                --testProperty "dft and inverse dft roundtrip" prop_dft_inverse_dft 
            ],
            testGroup "fft" [
                testProperty "N.fft equals the fft" prop_fft_equals_fft,
                testProperty "fft equals dft"       prop_fft_equals_dft,
                testProperty "sdft equals dft"      prop_sdft_equals_dft,
                testCase "test_getBlah"   test_getBlah,
                testCase "test_getEven"   test_getEven,
                testCase "test_getOdd"    test_getOdd
                --testCase "test_sdft_0"    test_sdft_0
                --testCase "test_combine"   test_combine,
                --testCase "test_top"       test_top,
                --testCase "test_bottom"    test_bottom,
                --testCase "test_fft"       test_fft,
                --testCase "test_decimate'" test_decimate',
                --testCase "test_twiddle"   test_twiddle
            ]
        ]
        
prop_dft_equals_n_dft :: [Complex Double] -> Bool
prop_dft_equals_n_dft xs = N.dft xs =~= dft xs

prop_dft_inverse_dft :: [Complex Double] -> Bool
prop_dft_inverse_dft xs = (invDft $ dft xs) =~= xs

newtype PowerOfTwoList a = PowerOfTwoList [a]
    deriving(Show, Eq, Read, Ord)
    
instance (Arbitrary a) => Arbitrary (PowerOfTwoList a) where
    arbitrary = do
        len <- choose (0, 8 :: Int)
        xs  <- vector (2 ^ len)
        return $ PowerOfTwoList xs 

prop_fft_equals_fft :: PowerOfTwoList (Complex Double) -> Bool
prop_fft_equals_fft (PowerOfTwoList xs) = N.fft xs =~= fft xs

prop_fft_equals_dft :: PowerOfTwoList (Complex Double) -> Bool
prop_fft_equals_dft (PowerOfTwoList xs) = fft xs =~= dft xs

--TODO to test this I need to start with a dft and then add one value and it should be the 
--same as doing two dfts
prop_sdft_equals_dft :: [Complex Double] -> Bool
prop_sdft_equals_dft     [] = True
prop_sdft_equals_dft (x:[]) = True
prop_sdft_equals_dft (x:xs) = sdft (dft xs) (head xs) x =~= dft ((tail xs) ++ [x])

test_getBlah = actual @?= expected where
    actual   = get_blah (\x -> x > 3) initial
    expected = [4,5,6]
    initial  = [0,1,2,3,4,5,6]
    
test_getEven = actual @?= expected where
    actual   = get_even initial
    expected = [0,2,4,6]
    initial  = [0,1,2,3,4,5,6]
    
test_getOdd  =  actual @?= expected where
    actual   = get_odd initial
    expected = [1,3,5]
    initial  = [0,1,2,3,4,5,6]
    
test_combine = actual @?= expected where
    actual   = combine sign 2 evens odds index
    expected = 1.0 :+ 0.0
    evens = 1.0 :+ 0.0
    odds  = 3.0 :+ 0.0
    index = 1
    sign  = 1
         

    
test_fft = actual @?= expected where
    actual   = fft [0.0, 2.0]
    expected = [1.0, 2.0]

--test_decimate = actual @?= expected where
--    actual   = decimate [0.0, 2.0]
--    expected = [0.0]

test_twiddle = actual @?= expected where
    actual   = twiddle [0.0, 2.0] [1.0, 3.0]
    expected = [0.0, 1.0]

{-
test_sdft_0 = actual @?= expected where
    actual   = sdft (fft initial) (head initial) new
    expected = fft $ (tail initial) ++ [new]
    initial = [1.0 :+ 0.0,1.0 :+ 0.0]
    new     = 2.0 :+ 0.0
-}    
--TODO verify that operations counts are what one would think
--write the sliding window version

--count_expression 



















