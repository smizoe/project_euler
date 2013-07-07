module Main where

import Data.Char(digitToInt)
-- import Data.Word(Word)
import qualified Data.Map as M

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

factorials :: M.Map Int Int
factorials = M.fromList $ zip [1..9] (map fact [1..9])

numIsSumDigitFactorial :: Int -> Bool
numIsSumDigitFactorial n = n == (sum . map (\x -> M.findWithDefault 1 (digitToInt x) factorials )) (show n)


main = (putStrLn . show) $ sum $ filter numIsSumDigitFactorial [3..9999999]