module Main where

import Data.Word(Word)
import Data.List(sort)

sumAmicables :: [Word] -> Word
sumAmicables = sum . (filter isAmicable)

isAmicable :: Word -> Bool
isAmicable n = (m /= 1) && (n == sumDivisors m) && m /= n
  where sumDivisors = sum . init . sort . divisorsOf
        m = sumDivisors n

divisorsOf :: Word -> [Word]
divisorsOf n = do
  cand <- [x | x <- [1..n] :: [Word], x^2 < n]
  miniFun cand
  where miniFun x = if (n `mod` x == 0)
                    then [x, n `div` x]
                    else []

main = putStrLn . show $ sumAmicables [2..10000]