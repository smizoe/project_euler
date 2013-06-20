module PE29 where

import Eratosthenes
import qualified Data.Set as Set(empty,size,insert)
import Control.Applicative((<*>))
    
smallPrimes :: [Int]
smallPrimes = map (fromIntegral :: Integer -> Int) $ takeWhile (< 100) primes 

numFactor :: Int -> Int -> Int
numFactor f n = (last . takeWhile (\x -> n `mod` f^x == 0)) [0..]

transformNum :: Int -> [Int]
transformNum n =  map (\x -> x $ n) funcs
    where funcs = map numFactor smallPrimes


ans = Set.size $ foldr Set.insert Set.empty $ (concat . (map applyPower)) $ map transformNum [2..100]
    where
      applyPower :: [Int] -> [[Int]]
      applyPower list = (map (\x -> map (*x)) [2..100]) <*> [list]