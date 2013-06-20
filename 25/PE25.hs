module PE25 where

fib :: [Integer]
fib = 1:1: zipWith (+) fib (tail fib)

ans = 1 + (length . takeWhile (\x -> (length . show) x < 1000)) fib