module JustBench.Measure
    (
    BenchResult (fromMeasured),
    benchPure,
    benchWeaklyPure
    ) where

      import Data.Int
      import qualified Criterion.Types as T
      import Criterion.Main
      import Criterion.Measurement
      import Control.Monad.Par.Class

      {- Class for types, which can contain elements of a measurement result and
         which can be created from measurement results
      -}
      class BenchResult r where
        fromMeasured :: T.Measured -> r

      {- Create Benchmark of a pure function with normal form evaluation -}
      benchPure :: (NFData b, BenchResult c) => Int -> (a -> b) -> a -> IO c
      benchPure it f arg = do
                            let
                              generalIt = fromIntegral it
                            (result, _) <- nf f arg `measure` generalIt
                            return . fromMeasured . measuredAverage it $ result

      {- Create Benchmark of a pure function with weak head normal form evaluation -}
      benchWeaklyPure :: (BenchResult c) => Int -> (a -> b) -> a -> IO c
      benchWeaklyPure it f arg = do
                                 let
                                    generalIt = fromIntegral it
                                 (result, _) <- whnf f arg `measure` generalIt
                                 return . fromMeasured . measuredAverage it $ result

      {- Function for creating the average measurement results for one iteration
       from an measurement m with n iterations -}
      measuredAverage :: Int -> T.Measured -> T.Measured
      measuredAverage n m = let
                              fracN = realToFrac . fromIntegral $  n
                            in T.Measured {
                                T.measTime = T.measTime m / fracN,
                                T.measCpuTime = T.measCpuTime m / fracN,
                                T.measCycles = round $ int64ToFrac (T.measCycles m) / fracN,
                                T.measIters = round $ int64ToFrac (T.measIters m) / fracN,
                                T.measAllocated = round $ int64ToFrac (T.measAllocated m) / fracN,
                                T.measNumGcs = round $ int64ToFrac (T.measNumGcs m) / fracN,
                                T.measBytesCopied = round $ int64ToFrac (T.measBytesCopied m) / fracN,
                                T.measMutatorWallSeconds = T.measMutatorWallSeconds m / fracN,
                                T.measMutatorCpuSeconds = T.measMutatorCpuSeconds m / fracN,
                                T.measGcWallSeconds = T.measGcWallSeconds m / fracN,
                                T.measGcCpuSeconds = T.measGcCpuSeconds m / fracN
                              }
                      where
                        int64ToFrac :: (Fractional a) => Int64 -> a
                        int64ToFrac = realToFrac . fromIntegral
