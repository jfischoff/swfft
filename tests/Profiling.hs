module Profiling where
import qualified Numeric.FFT as N
import Math.FFT.Approximate
import Math.FFT.Numbers
import Math.FFT.FFT
import Math.FFT.DFT
import Data.Complex    
    
import Criterion.Main
import Criterion
import Criterion.Config
import Data.Monoid

dummyData0 = map (:+ (0.0 :: Double)) [0.0 .. 511.0]
dummyData1 = map (:+ (0.0 :: Double)) [0.0 .. 1023.0]
dummyData2 = map (:+ (0.0 :: Double)) [0.0 .. 2047.0]

window count xs = take count
--TODO make the window function
--test performance
--test error

windowData0 = window 256 dummyData0

myConfig = defaultConfig { cfgReport = Last $ Just "profile.html" }

main = defaultMainWith myConfig (return ()) [
          bgroup "fft0" [
             bench "N.fft" $ whnf N.fft dummyData0
           , bench "N.dft" $ whnf N.dft dummyData0
           , bench "dft"   $ whnf dft   dummyData0
           , bench "fft"   $ whnf fft   dummyData0
           ],
           bgroup "fft1" [
              bench "N.fft" $ whnf N.fft dummyData1
            , bench "N.dft" $ whnf N.dft dummyData1
            , bench "dft"   $ whnf dft   dummyData1
            , bench "fft"   $ whnf fft   dummyData1
            ],
            bgroup "fft2" [
               bench "N.fft" $ whnf N.fft dummyData2
             , bench "N.dft" $ whnf N.dft dummyData2
             , bench "dft"   $ whnf dft   dummyData2
             , bench "fft"   $ whnf fft   dummyData2
             ],
             bgroup "sliding window 0" [
                bench "fft"  $ whnf (map fft) (window 256 dummyData0),
                bench "dft"  $ whnf (map dft) (window 256 dummyData0),
                bench "sdft" $ whnf slideDFT  (window 256 dummyData0)
             ]
        ]