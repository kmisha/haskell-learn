strange :: Int -> Int
strange i = 22

main :: IO ()
-- main = print . strange $  (2 / 0)
main  = print ([(n, 2^n) | n <- [0..19]])