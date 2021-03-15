module Church where

zero f x = x
one  f x = f x
two  f x = f $ f x

inc n = \f x -> f (n f x)

unchurch n = n (+ 1) 0
 