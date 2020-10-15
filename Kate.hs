module Kate where

generateSequnses :: String -> Int -> [Int]
generateSequnses [] _ = []
generateSequnses (x:xs) n = (read (x : take n xs) :: Int) : generateSequnses xs n
