module Main where

import Numeric(showIntAtBase)

numsConsidered = filter odd [1..1000000]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isDoublePalindrome :: Int -> Bool
isDoublePalindrome n = (isPalindrome . showBin) n && (isPalindrome . show) n
  where showBin x =  (showIntAtBase 2 (\y -> if (y==0)
                                             then '0'
                                             else '1') x) ""


ans = sum $ filter isDoublePalindrome numsConsidered

main = (putStrLn . show) ans
