module PE16 where

digitSum :: Integer -> Integer
digitSum n = sum . map (read . (\x -> [x]) ) $ show n

ans = digitSum $ 2^1000