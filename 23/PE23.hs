module Main where

import qualified Data.Set as Set(member,fromList)

isAbundant :: Int -> Bool
isAbundant n = helper 1 $ takeWhile (\x -> x^2 <= n) [2..]
    where
      helper _ [] = False
      helper k (x:xs) | r /= 0 = helper k xs
                      | l > n  = True
                      | otherwise = helper l xs
          where (q,r) = n `quotRem` x
                l = k + x + if(q /= x)
                            then q
                            else 0

abundantNums = filter isAbundant [12..]

smallerAbundNums = Set.fromList $! takeWhile (<= 28123) abundantNums

isDecomposable n = any (\x -> (n - x) `Set.member` smallerAbundNums) candAbunds
    where candAbunds = takeWhile (\x -> 2 * x <= n) abundantNums

main = (putStrLn . show . sum) $! filter (not . isDecomposable) [1..28123]

