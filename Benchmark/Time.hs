module Benchmark.Time
    (
      benchPure,
      benchWeaklyPure
    ) where
      import Criterion.Main
      import Criterion.Measurement
      import Control.Monad.Par.Class

      benchPure :: (NFData b) => Int -> (a -> b) -> a -> BenchResult
      benchPure it f = toBenchResult . (`measure` it) . nf f

      benchWeaklyPure :: Int -> (a -> b) -> a -> BenchResult
      benchWeaklyPure it f = toBenchResult . (`measure` it) . whnf f
