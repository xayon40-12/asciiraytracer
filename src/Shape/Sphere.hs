module Shape.Sphere where

import Shape
import Vec

-- Sphere Pos Radius
data Sphere = Sphere Vec Double deriving (Show)

instance Collidable Sphere where
  cast (Sphere ps r) (Ray pr d) = case delta of
    -- t<0 means behind so no collision considered
    _d | _d >= 0 && t > 0 -> Just $ CNode pos n t
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

sphere :: Vec -> Double -> Shape
sphere pos radius = Shape $ Sphere pos radius
