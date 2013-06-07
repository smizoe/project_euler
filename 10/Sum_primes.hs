module Sum_primes where

primes :: [Int]
primes = 2:[x| x <- [3..], isPrime x]

isPrime :: Int -> Bool
isPrime x = all (\f -> x `mod` f > 0) (factorsToTry x)
        where factorsToTry x = takeWhile (\p -> p^2 <= x) primes

ans :: Int 
ans = foldr (+) 0 $ takeWhile (\p -> p < 2000000) primes
