module PE67 where

import PE18
import System.IO

ans = do
  handle <- openFile "triangle.txt" ReadMode
  contents <- hGetContents handle
  let inputPE67 = processTriangle contents
  return (head . (foldr1 largerPath) $ inputPE67)