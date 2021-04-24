module Shape.Sphere where

import Render
import Render.CNode
import Render.Color
import Shape
import Vec

data Sphere = Sphere
  { _pos :: Vec,
    _radius :: Double,
    _color :: Color
  }
  deriving (Show)

instance Collidable Sphere where
  cast (Sphere ps r col) (Ray pr d) = case delta of
    -- t<0 means behind so no collision considered
    _d | _d >= 0 && t > 0 -> Just $ CNode pos n col t
    _ -> Nothing
    where
      p = pr .-. ps
      a = d .*. d
      b = d .*. p * 2
      c = p .*. p - r * r
      delta = b * b - 4 * a * c
      sd = sqrt delta
      t = (if sd < - b then - b - sd else - b + sd) / (2 * a)
      pos = pr .+. t *. d
      n = normalize $ pos .-. ps
  contains (Sphere ps r _) v = norm2 (v .-. ps) <= r * r

sphere :: Vec -> Double -> Color -> Shape
sphere pos radius c = Shape $ Sphere pos radius c
