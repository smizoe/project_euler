module PE33 where

import Data.Ratio
import Data.List(intersect,(\\))
import qualified Control.Applicative as App((<*>))

isNiceFraction :: Int -> Int -> Bool
isNiceFraction n d = hasOneDigitInCommon && n % d == (removeCommon n) % (removeCommon d)
  where
    commonDigits = (show n) `intersect` (show d)
    hasOneDigitInCommon = 1 == length commonDigits
    removeCommon :: Int -> Int
    removeCommon num = (read :: [Char] -> Int) $ (show num) \\ commonDigits

ans = (denominator . product) [x % y|x <- cands, y <- cands, x < y, isNiceFraction x y]
  where cands = map (+) [1..9] App.<*> [10,20..90]