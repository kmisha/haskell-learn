module Demo where

data Vehicle = Car | Bike

instance Eq Vehicle where
  Car  == Car  = True
  Bike == Bike = True
  _    == _    = False


class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString True  = "true"
  toString False = "false"

instance Printable () where
  toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) = (x == y) && (xs == ys)
  _      == _      = False