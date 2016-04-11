module JustBench.Measure
    (
    measuredAverage
    ) where

      import Criterion.Types
      import Data.Int

      {- Function for creating the average measurement results for one iteration
       from an measurement m with n iterations -}
      measuredAverage :: Int -> Measured -> Measured
      measuredAverage n m = let
                              fracN = realToFrac . fromIntegral $  n
                            in Measured {
                                measTime = measTime m / fracN,
                                measCpuTime = measCpuTime m / fracN,
                                measCycles = round $ int64ToFrac (measCycles m) / fracN,
                                measIters = round $ int64ToFrac (measIters m) / fracN,
                                measAllocated = round $ int64ToFrac (measAllocated m) / fracN,
                                measNumGcs = round $ int64ToFrac (measNumGcs m) / fracN,
                                measBytesCopied = round $ int64ToFrac (measBytesCopied m) / fracN,
                                measMutatorWallSeconds = measMutatorWallSeconds m / fracN,
                                measMutatorCpuSeconds = measMutatorCpuSeconds m / fracN,
                                measGcWallSeconds = measGcWallSeconds m / fracN,
                                measGcCpuSeconds = measGcCpuSeconds m / fracN
                              }
                      where
                        int64ToFrac :: (Fractional a) => Int64 -> a
                        int64ToFrac = realToFrac . fromIntegral
