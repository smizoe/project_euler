module NumRoute where

import Data.Array

len = 20

makeValid :: (Int, Int) -> (Int, Int)
makeValid pair | fst pair < 0 || snd pair <0 = (0,0)
               | otherwise                   = pair

dpLattice :: Array (Int, Int) Int
dpLattice = array ((0,0),(len ,len)) ( ((0,0),0) : ((1,0),1) : ((0,1),1) : [ ((x,y), comb) | x <- [0..len],
                                                                                           y <- [0..len],
                                                                                           not $ (x,y) `elem` [(0,0),(1,0),(0,1)],
                                                                                           let comb = dpLattice !  makeValid (x-1, y) +  dpLattice ! makeValid (x, y-1)])

ans = dpLattice ! (len,len)