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
import Math.FFT.SlidingWindow
import Debug.Traced
import Data.Complex hiding (phase)
import qualified Numeric.FFT as N
import Math.FFT.Approximate

amp       = 1.0
period    = 1.0
phase     = 1.0
delta     = 1.0 
startTime = 1.0
count     = 4


main = defaultMain tests

tests = [
            testGroup "dft" [
                testProperty "dft equals N.dft" prop_dft_equals_n_dft,
                testProperty "dft and inverse dft roundtrip" prop_dft_inverse_dft 
            ],
            testGroup "fft" [
                testProperty "N.fft equals the fft" prop_fft_equals_dft,
                testCase "test_getBlah"   test_getBlah,
                testCase "test_getEven"   test_getEven,
                testCase "test_getOdd"    test_getOdd
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

prop_fft_equals_dft :: PowerOfTwoList (Complex Double) -> Bool
prop_fft_equals_dft (PowerOfTwoList xs) = N.fft xs =~= fft xs

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
    actual   = combine evens odds index sign
    expected = 1
    evens = [1.0, 2.0]
    odds  = [3.0, 4.0]
    index = 1
    sign  = 1
         
test_top = actual @?= expected where
    actual   = top [0.0, 2.0] [1.0, 3.0] 1
    expected = 1
     
test_bottom = actual @?= expected where
    actual = bottom [0.0, 2.0] [1.0, 3.0] 3
    expected = 1
    
test_fft = actual @?= expected where
    actual   = fft [0.0, 2.0]
    expected = [1.0, 2.0]

test_decimate' = actual @?= expected where
    actual   = decimate' [0.0, 2.0]
    expected = [0.0]

test_twiddle = actual @?= expected where
    actual   = twiddle [0.0, 2.0] [1.0, 3.0]
    expected = [0.0, 1.0]
    
--TODO verify that operations counts are what one would think
--write the sliding window version



















