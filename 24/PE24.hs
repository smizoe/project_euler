module PE24 where

import Data.Char(intToDigit)
import Data.List((\\))

type PermRemDigit = ([Int], Int, Int)

determineADigit :: PermRemDigit -> PermRemDigit
determineADigit t@(perm,rem,d) | d == 0 = t
                               | otherwise = ((rest!!q):perm, r, d-1)
    where
      rest = [0..9] \\ perm
      fact n | n==1 || n==0 = 1
             | otherwise = n * fact (n-1)
      (q,r) = rem `quotRem` fact (d-1)
      
ans = ((map intToDigit) . reverse . fstone) $ (foldr1 (.) (replicate 10 determineADigit)) ([],1000000 - 1,10)
    where fstone (a,_,_) = a