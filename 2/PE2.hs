module PE2 where
import Control.Applicative((<*>))

fibSeq :: [Int]
fibSeq = 1:2:[x + y| (x,y) <- zip fibSeq (tail fibSeq)]

ans =  sum $ filter even $ takeWhile (< 4000000) fibSeq

