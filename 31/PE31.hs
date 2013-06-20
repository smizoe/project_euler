module PE31 where

nextStep :: [Int] -> Int -> Int
nextStep coins restToGo
    | restToGo < 0  = 0
    | restToGo == 0 = 1
    | otherwise     = sum $ map (\x -> nextStep (filter (>=x) coins) (restToGo - x)) coins

ans = 1 + nextStep [1,2,5,10,20,50,100] 200