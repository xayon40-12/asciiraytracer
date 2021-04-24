module Shape.Sphere where

import Control.Monad
import Render
import Render.CNode
import Render.Color
import Shape
import Utils
import Vec

data Sphere = Sphere
  { _pos :: Vec,
    _radius :: Double,
    _color :: Color
  }
  deriving (Show)

instance Collidable Sphere where
  cast (Sphere ps r col) (Ray pr d) = res
    where
      p = pr .-. ps
      a = d .*. d
      b = d .*. p * 2
      c = p .*. p - r * r
      delta = b * b - 4 * a * c
      sd = sqrt delta
      res = do
        guard $ delta >= 0
        t <- (\x -> (- b + x) / (2 * a)) <$> [- sd, sd]
        guard $ t > 0
        let pos = pr .+. t *. d
        let n = normalize $ pos .-. ps
        return $ CNode pos n col t
  contains (Sphere ps r _) v = norm2 (into v .-. ps) <= r * r

sphere :: Vec -> Double -> Color -> Shape
sphere pos radius c = Shape $ Sphere pos radius c
