module JustBench.Measure.Time
    (
      TimeBenchResult (..)
    ) where

      import JustBench.Measure
      import Criterion.Types

      data TimeBenchResult = TimeBenchResult {
                              time :: Double,
                              cpuTime :: Double,
                              cycles :: Integer,
                              iterations :: Integer
                             }
                             deriving (Show)

      instance BenchResult TimeBenchResult where
        fromMeasured m = TimeBenchResult {
                          time = measTime m,
                          cpuTime = measCpuTime m,
                          cycles = fromIntegral $ measCycles m,
                          iterations = fromIntegral $ measIters m
                         }
