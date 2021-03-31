module Dotted where

data Dotted = Dotted Integer

instance Show Dotted where
  show (Dotted n) = "." ++ show n

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
    | maxBound == x = minBound
    | otherwise     = succ x

  spred :: a -> a
  spred x
    | minBound == x = maxBound
    | otherwise     = pred x
