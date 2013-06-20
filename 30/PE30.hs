module PE30 where

-- we only need to verify numbers less than 354294, since 9^5 * 6 = 354294;
-- no six-digit number can have the sum of 5th-power of its digits exceeding 354294

import Data.Char(digitToInt)

sum5thPowDigits :: Int -> Int
sum5thPowDigits n = (sum . (map ((^5) . digitToInt)) . show) n

ans = sum $ filter (\x -> x == sum5thPowDigits x) [2..324294]