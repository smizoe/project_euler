module PE12 where


numFactors :: [Int] -> Int  -> Int -> [Int]
numFactors _ numFact 1 = [numFact]
numFactors (f:fs) numFact n | n `mod` f == 0 = numFactors (f:fs) (numFact + 1) (n `div` f)
                          | numFact > 0    = numFact : numFactors fs 0 n
                          | otherwise      = numFactors fs 0 n


numFactList :: Int -> [Int]
numFactList n = numFactors [2..] 0 n

factLists :: [[Int]]
factLists = map numFactList [2..]

divideBy2 :: [[Int]] -> [[Int]]
divideBy2 ((x:xs) : ys : zs) =  ((x - 1):xs) : ys : divideBy2 zs


numDivisors' :: [Int]
numDivisors' = map (\x -> product $ map (+1) x) $ divideBy2 factLists

numDivisorsAndNum = zip (zipWith (*) numDivisors' (drop 1 numDivisors'))
                        (zipWith (\x y -> x * y `div` 2) [2..] [3..])

