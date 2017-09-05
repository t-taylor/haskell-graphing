module Linear
  ( collatz
  , intftodouble
  ) where

-- Collatz conjecture
collatz :: Int -> Int
collatz = coll 0
  where coll acc 1 = acc
        coll acc n = coll (acc + 1) (cf n)
        cf n
          | n `mod` 2 == 0 = n `div` 2
          | otherwise = 3 * n + 1

-- Converts an int function to a scaled double function
intftodouble :: (Int -> Int) -> Double -> (Double -> Double)
intftodouble f res
  = fromIntegral . f . round . (* (1/res))
