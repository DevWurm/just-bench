module Benchmark.Time
    (
      benchPure,
      benchWeaklyPure,
      TimeBenchResult (..)
    ) where

      import Benchmark
      import qualified Criterion.Types as T
      import Criterion.Main
      import Criterion.Measurement
      import Control.Monad.Par.Class

      data TimeBenchResult = TimeBenchResult {
                              time :: Double,
                              cpuTime :: Double,
                              cycles :: Integer,
                              interations :: Integer
                             }
                             deriving (Show)

      fromTimeMeasure :: T.Measured -> TimeBenchResult
      fromTimeMeasure m = TimeBenchResult {
                            time = T.measMutatorWallSeconds m,
                            cpuTime = T.measMutatorCpuSeconds m,
                            cycles = fromIntegral $ T.measCycles m,
                            interations = fromIntegral $ T.measIters m
                          }

      benchPure :: (NFData b) => Int -> (a -> b) -> a -> IO TimeBenchResult
      benchPure it f arg = do
                            let
                              generalIt = fromIntegral it
                            (result, _) <- nf f arg `measure` generalIt
                            return . fromTimeMeasure . measuredAverage it $ result

      benchWeaklyPure :: Int -> (a -> b) -> a -> IO TimeBenchResult
      benchWeaklyPure it f arg = do
                                 let
                                    generalIt = fromIntegral it
                                 (result, _) <- whnf f arg `measure` generalIt
                                 return . fromTimeMeasure . measuredAverage it $ result
