module PE28 where

oneFourthCircumference = (concat . map (replicate 4)) [2,4..]
                    
ans = sum $ scanl (+) 1 $ takeWhile (< 1001) oneFourthCircumference