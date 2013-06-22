module PE8 where

import Data.Char(isSpace,digitToInt)
import System.IO

findMaxProduct :: Int -> String -> Int
findMaxProduct n [] = n
findMaxProduct n str = findMaxProduct larger $ tail str
  where larger = max n (fiveProd str)
        fiveProd = product . (map (digitToInt)) . (take 5)
  
ans = do
  handle <- openFile "number.txt" ReadMode
  contents <- hGetContents handle
  let numString =  filter (not. isSpace) contents
  return $ findMaxProduct 0 numString