strange :: Int -> Int
strange i = 22

data IPAddress = IPv4 String
               | IPv4Localhost
               | IPv6 String
               | IPv6Localhost

-- checkIP :: IPAddress -> String
-- checkIP _ adr = "IP is " ++ adr ++ "."

generate :: String -> [String]
generate [] = []
generate (x: xs) = x : take 5 xs

main :: IO ()
-- main = print . strange $  (2 / 0)
-- main  = print ([(n, 2^n) | n <- [0..19]])
-- main = putStrLn . checkIP $ IPv6 "192.168.1.2"
main = print . (map generate) $ "1234567890"