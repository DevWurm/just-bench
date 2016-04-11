module JustBench.Measure.Memory
    (
      MemoryBenchResult (..)
    ) where

      import JustBench.Measure
      import Criterion.Types

      data MemoryBenchResult = MemoryBenchResult {
                                allocated :: Integer,
                                copied :: Integer,
                                iterations :: Integer
                               }
                               deriving (Show)

      fromMemoryMeasure :: Measured -> MemoryBenchResult
      fromMemoryMeasure m = MemoryBenchResult {
                              allocated = fromIntegral $ measAllocated m,
                              copied = fromIntegral $ measBytesCopied m,
                              iterations = fromIntegral $ measIters m
                            }

      instance BenchResult MemoryBenchResult where
        fromMeasured m = MemoryBenchResult {
                                allocated = fromIntegral $ measAllocated m,
                                copied = fromIntegral $ measBytesCopied m,
                                iterations = fromIntegral $ measIters m
                              }
