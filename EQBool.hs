module Demo where

data Vehicle = Car | Bike

instance Eq Vehicle where
  Car  == Car  = True
  Bike == Bike = True
  _    == _    = False
