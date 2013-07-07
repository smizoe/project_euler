module Main where

import Eratosthenes
import qualified Data.Set as S(Set, intersection, fromList, map, size)

inQuestion = takeWhile (< 1000000) primes

rotate :: Ord a => [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]

main = (putStrLn . show . S.size) (foldl1 op (replicate 6 nums))
  where nums = S.fromList $ map show inQuestion
        op :: S.Set String -> S.Set String -> S.Set String
        x `op` y = (S.map rotate x) `S.intersection` y