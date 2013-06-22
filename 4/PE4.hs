module PE4 where

isPalidrome :: String -> Bool
isPalidrome s = s == reverse s

ans = foldl max 0 cand
  where cand = [prod | x <- [100..999], y <- [x..999], let prod = x * y, isPalidrome $ show prod]