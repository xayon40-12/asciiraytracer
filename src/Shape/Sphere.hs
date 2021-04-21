module Shape.Sphere where

import Shape
import Vec

type Radius = Double

data Sphere = Sphere Vec Radius

instance Shape Sphere where
  cast (Sphere ps r) (Ray pr d) = case delta of
    0 -> [CNode pos1 n1 t1]
    _d | _d>0 -> [CNode pos1 n1 t1,CNode pos2 n2 t2]
    _ -> []
    where p = pr .-. ps
          a = d .*. d
          b = d .*. p * 2
          c = p .*. p - r*r
          delta = b*b - 4*a*c
          sd = sqrt delta
          t1 = (b-sd)/(2*a)
          t2 = (b+sd)/(2*a)
          pos1 = pr .+. t1 *. d
          pos2 = pr .+. t2 *. d
          n1 = normalize $ pos1 .-. ps
          n2 = normalize $ pos2 .-. ps
