module Eratosthenes where

-- the following algorithm is taken from "The Genuine Sieve of Eratosthenes" by Melissa E. O'Neill

import qualified Data.Map as Map

primes = sieve [2..]

isPrime x = x == (head . (dropWhile (< x))) primes

sieve xs = sieve' xs Map.empty
    where
      sieve' [] table = []
      sieve' (x:xs) table =
          case Map.lookup x table of
            Just facts -> sieve' xs (foldl reinsert (Map.delete x table) facts)
            Nothing -> x : sieve' xs (Map.insert (x*x) [x] table)
          where
            reinsert table prime = Map.insertWith (++) (x+prime) [prime] table