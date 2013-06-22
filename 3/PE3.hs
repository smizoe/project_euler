module PE3 where

import Eratosthenes

removeFactorFrom :: Integer -> Integer -> Integer
removeFactorFrom p n | r == 0 = removeFactorFrom p q
                     | otherwise = n
  where (q,r) = n `quotRem` p
        
findLargestPrimeFactor :: Integer -> Integer
findLargestPrimeFactor n = oneStep primes n 
  where oneStep ps m | next == 1 = p
                     | otherwise = oneStep rest next
          where (p : rest) = ps
                next = removeFactorFrom p m


ans = findLargestPrimeFactor 600851475143