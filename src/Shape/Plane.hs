module Shape.Plane where

import Render
import Render.CNode
import Render.Color
import Shape
import Utils
import Vec

data Plane = Plane
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color
  }
  deriving (Show)

instance Collidable Plane where
  cast (Plane pp dp c) (Ray pr dr) = [CNode pos dr c t | dpr /= 0 && t > 0]
    where
      p = (pr .-. pp) .*. dp
      dpr = dp .*. dr
      t = - p / dpr
      pos = pr .+. t *. dr
  contains (Plane pp dp _) v = (into v .-. pp) .*. dp <= 0

plane :: Vec -> Vec -> Color -> Shape
plane pos dir c = Shape $ Plane pos dir c
