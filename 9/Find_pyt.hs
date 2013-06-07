module Find_pyt where

candidate :: [(Int,Int,Int)]
candidate = [(a,b,c)| c <- [409..], b <- [1..c], a <- [1..b], a+b+c==1000, a+b>c, a^2+b^2==c^2]

prod3 :: (Int,Int,Int) -> Int
prod3 (a,b,c) = a * b* c