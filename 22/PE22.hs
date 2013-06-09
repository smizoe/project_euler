module PE22 where

import Data.Word(Word)
import Data.Char(ord, isSpace)
import Data.List.Split(splitOn)
import Data.List(sort)
import System.IO

stringToScore :: String -> Word
stringToScore str = (sum . (map (\x -> fromIntegral (ord x - ord 'A' + 1) :: Word))) str 

ans = do
    handle <- openFile "names.txt" ReadMode
    content <- hGetContents handle
    let inputPE22 = sort $ splitOn "," $ filter (\c -> c /= '"') content
    return $ sum $ zipWith (*) (map stringToScore inputPE22) [(1 :: Word)..((fromIntegral . length) inputPE22)]