module Main where

import Eratosthenes
import qualified Data.List as L(intersect)

{-
intention:

 we construct truncatable primes in a bottom-up way; we start from
 [2,3,5,7], and make two lists. One is constructed by always appending one digit
 to a number; another is made by always prepending a digit.
 After every appending and prepending, we try taking intersection of the two lists.
 If they have some elements in common, then they are truncable primes.
 Otherwise there is no truncable primes in the two lists in that round.

 Further, truncatable primes can't have 0
 So we don't consider prepending or appending 0
-}

prependDigit :: String -> [String]
prependDigit xs = map (\f -> f xs) (map (:) ['1'..'9'])

appendDigit :: String -> [String]
appendDigit xs = map (\f -> f xs) (map (\y -> (++ [y])) ['1'..'9'])

truncPrimes = searchNext inList inList
  where
    inList = map (\x -> [x]) ['2','3','5','7']
    searchNext xs ys | length zs > 0 = (map (read :: String -> Integer) zs) ++ searchNext x2s y2s
                         | otherwise     = searchNext x2s y2s
          where x2s = filter (isPrime . (read :: String -> Integer)) $ (concat . (map prependDigit)) xs
                y2s = filter (isPrime . (read :: String -> Integer)) $ (concat . (map appendDigit)) ys
                zs  = x2s `L.intersect` y2s


ans = (sum . (take 11)) $ truncPrimes