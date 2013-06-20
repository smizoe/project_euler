module PE26 where

{-
  say we have the following number with recurring cycle (r), where length of r is l:
  q = 0.(a1)(a2)...(an)(r)

  if we multiply q by 10^l and take difference from q, we have a number without recurring cycle;
  (10^l -1) * q is a number with decimal fraction part of finite length.

  So, it seems that we can find l by looking at the length of the decimal fraction part...
-}

findLowestPower :: Integer -> Integer
findLowestPower n = (head . dropWhile (\x -> (10^x -1) `mod` n /=0)) [1..]

-- `simplify` removes the factor of 2 and 5 from the given number
simplify :: Integer -> Integer
simplify n | n `mod` 2 == 0 = simplify (n `div` 2)
           | n `mod` 5 == 0 = simplify (n `div` 5)
           | otherwise      = n


ans = snd $ foldl1 max [(findLowestPower $ simplify x, x)|x <- [1..999]]