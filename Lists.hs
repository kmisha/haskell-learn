module Lists where

nTimes:: a -> Int -> [a]
nTimes e 0 = []
nTimes e n = e : nTimes e (n - 1)
