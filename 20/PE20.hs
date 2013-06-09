module PE20 where

import Data.Char(digitToInt)

digitSum :: [Integer] -> Int
digitSum ns = (sum . (map digitToInt) . show . product) ns

ans = digitSum ([1..100] :: [Integer])
