module Lists where

import Prelude hiding (length, (++), null)
import Data.Char

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


oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (x: xs)
    | odd x     = x : oddsOnly xs
    | otherwise = oddsOnly xs


isPalindrome :: Eq a => [a] -> Bool
isPalindrome (x: xs)
  | x == last xs = isPalindrome (init (tail xs))
  | otherwise    = False
  
readDigits :: String -> (String, String)
readDigits = span isDigit

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj first second xs = filter (\x -> first x || second x) xs

quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x^2, x^3])

delAllUpper :: String -> String
delAllUpper = unwords . filter (\x -> not (all isUpper x)) . words

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream) 

data Odd = Odd Integer deriving (Eq, Show)

instance Enum Odd where
  toEnum n = (Odd (toInteger n))
  fromEnum (Odd n) = fromInteger n
  succ (Odd n) = (Odd (n + 2))
  pred (Odd n) = (Odd (n - 2))
  enumFrom = iterate succ
  enumFromThen (Odd x) (Odd y) = map Odd [x, y ..]
  enumFromTo (Odd x) (Odd y) = map Odd [x, x + 2 .. y]
  enumFromThenTo (Odd x) (Odd y) (Odd z) = map Odd [x, y .. z]

coins = [2, 3, 7]

change :: (Ord a, Num a) => a -> [[a]]
change n | n == 0 = [[]]
         | otherwise = [x : exchanged | x <- [2, 3, 7], x <= n, exchanged <- change $ n - x]
