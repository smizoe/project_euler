module PE32 where

-- we consider (4-digit number) x (1-digit number) and (2-digit number) x (3-digit number);
-- since we would like to use all of [1-9] and each for only once, we don't need to consider (3-digit) x (3-digit)

import Data.List(nub,delete,sort)

-- determineDigits enumerates 5-digit numbers each of whose digits is distinct
-- the argument is a string ("123456789")
determineDigits :: String -> [String]
determineDigits xs | length xs <= 5 = map (\x -> [x]) xs
                   | otherwise     = concat $ map (\x -> map (x:) $ determineDigits (delete x xs)) xs


calcProduct :: Int -> String -> Int
calcProduct n xs = num1 * num2
    where (first, second) = splitAt n xs
          num1 = read first :: Int
          num2 = read second :: Int

returnIfValid :: String -> [Int]
returnIfValid xs = do
  func <- [calcProduct 4, calcProduct 3]
  let prod = func xs
  if (sort (xs ++ show prod) == "123456789")
  then [prod]
  else []

ans = (sum . nub . concat) $ map returnIfValid $ determineDigits "123456789"