module Main where

import Data.List(foldl')

collatz :: Integer -> Integer
collatz n | seq n False = undefined
collatz 1 = 1
collatz n | odd n  = 3 * n +1
          | even n = n `div` 2

collatzSeq :: Integer -> [Integer]
collatzSeq n 
    | n == 1    = [1]
    | otherwise = n : (collatzSeq . collatz) n


ans = foldl' (\p1 p2 -> if snd p1 < (snd p2) 
                        then p2 
                        else p1  )  (0,0) $map (\n -> (n,(length . collatzSeq) n)) [1..1000000]
main = (putStrLn . show) ans      