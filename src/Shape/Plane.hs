module Shape.Plane where

import Shape
import Vec

-- Plane Pos Dir
data Plane = Plane Vec Vec Color deriving (Show)

instance Collidable Plane where
  cast (Plane pp dp c) (Ray pr dr) = if dpr /= 0 && t > 0 then Just $ CNode pos dr c t else Nothing
    where
      p = (pr .-. pp) .*. dp
      dpr = dp .*. dr
      t = - p / dpr
      pos = pr .+. t *. dr

plane :: Vec -> Vec -> Color -> Shape
plane pos dir c = Shape $ Plane pos dir c
