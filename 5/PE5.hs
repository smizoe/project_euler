module PE5 where

import Eratosthenes
import qualified Control.Applicative as App((<*>))

countFactorOf :: Int -> Int  -> Int
countFactorOf p n = countFactorIn 0 n
  where countFactorIn counter num | r == 0 = countFactorIn (counter + 1) q
                                  | otherwise = counter
          where (q,r) = num `quotRem` p
ans = product $ zipWith (^) smallPrimes  factorsOfSmallestNum
      where
        factorsOfSmallestNum = foldl1 (zipWith (max)) $ map (\x -> funs App.<*> [x]) [2..20]
        funs = map countFactorOf smallPrimes
        smallPrimes = map (fromIntegral :: Integer -> Int) $ takeWhile (< 20) primes