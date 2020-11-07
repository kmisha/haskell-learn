module RScholl where

identity x = x      -- I combinator
constant x y = x    -- K combinator
s f g x = f x (g x) -- S combinator

apply f x = f x

compose f g = \x -> f (g x)



