module PE27 where

-- by looking at the case of n = 0, b must be prime.
-- by looking at the case of n = 1, a must be odd.

import qualified Control.Applicative as App((<*>),liftA2)

import Eratosthenes

isPrime :: Int -> Bool
isPrime n | nn == (head . dropWhile (< nn)) primes = True
          | otherwise                            = False
          where nn = fromIntegral n

candAs :: [Int]
candAs = [id, negate] App.<*> [1,3..999]

candBs :: [Int]
candBs = [id, negate] App.<*> (map fromIntegral $ takeWhile (<1000) primes)

maxN :: Int -> Int -> Int
maxN a b = (length . takeWhile (\x -> polyIsPrime x)) [0..]
    where polyIsPrime n = isPrime (n^2 + a * n + b)

ans= snd $ foldl1 max $ App.liftA2 (\x y -> (maxN x y, x*y)) candAs candBs