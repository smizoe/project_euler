module PE17 where

import qualified Data.Map as Map
import Control.Applicative

{-
  In the following, we assume that the number to be parsed ranges from 1 to 9999.
-}


-- list of the # of letters in words expressing numbers less than 20 
lessThan20 = [3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8]

-- list of the # of letters in words expressing ?0, where ? ranges from 2 to  9
ties = [6, 6, 5, 5, 5, 7, 6, 6]

numLettersLessThan20 = Map.fromList (zip [1..19] lessThan20)
numLettersTies = Map.fromList (zip [2..9] ties)

funForThousands n = Map.findWithDefault 0 thousands numLettersLessThan20 + 8 * if thousands == 0 
                                                                           then 0
                                                                           else 1
    where thousands = n `div` 1000

funForHundreds n = Map.findWithDefault 0 hundreds numLettersLessThan20 + 7 * if hundreds == 0 
                                                                             then 0
                                                                             else 1
    where hundreds = n `mod` 1000 `div` 100

funForTensAndLess n | twoDigits < 20 = Map.findWithDefault 0 twoDigits numLettersLessThan20
                    | otherwise      = Map.findWithDefault 0 (twoDigits `div` 10) numLettersTies + Map.findWithDefault 0 (twoDigits `mod` 10) numLettersLessThan20
    where twoDigits = n `mod` 100

funForAnd n | q > 0 && r /= 0 = 3
            | otherwise        = 0
            where (q, r) = n `quotRem` 100

funLists :: [Int -> Int]
funLists = [funForThousands, funForHundreds, funForTensAndLess, funForAnd]

numLetters :: Int -> Int
numLetters n = sum $ funLists <*> [n]

ans = sum $ funLists <*> [1..1000]