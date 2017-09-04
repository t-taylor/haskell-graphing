module Collatz
  ( collatz
  , gcollatz
  ) where

collatz :: Int -> Int
collatz x = coll 0 x
  where coll acc 1 = acc
        coll acc n = coll (acc + 1) (cf n)
        cf n
          | n `mod` 2 == 0 = n `div` 2
          | otherwise = 3 * n + 1

gcollatz :: Double -> Double -> Double
gcollatz res
  = fromIntegral . collatz . floor . (* (1/res))
