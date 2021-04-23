module Shape.Plane where

import Object
import Shape
import Vec

data Plane = Plane
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color
  }
  deriving (Show)

instance Collidable Plane where
  cast (Plane pp dp c) (Ray pr dr) = if dpr /= 0 && t > 0 then Just $ CNode pos dr c t else Nothing
    where
      p = (pr .-. pp) .*. dp
      dpr = dp .*. dr
      t = - p / dpr
      pos = pr .+. t *. dr
  contains (Plane pp dp _) v = (v .-. pp) .*. dp >= 0

instance Object_ Plane where
  position p = Shape.Plane._pos p

plane :: Vec -> Vec -> Color -> Shape
plane pos dir c = Shape $ Plane pos dir c
