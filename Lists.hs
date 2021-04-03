module Lists where

import Prelude hiding (length, (++), null)

nTimes:: a -> Int -> [a]
nTimes e 0 = []
nTimes e n = e : nTimes e (n - 1)

length :: [a] -> Int
length []      = 0
length (x: xs) = 1 + length xs

(++) :: [a] -> [a] -> [a]
(++) [] as     = as
(++) (a: as) bs = a : as ++ bs

null :: [a] -> Bool
null [] = True
null _  = False