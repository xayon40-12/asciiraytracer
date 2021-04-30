module Shape.Collections.Location where

import Render
import Render.CNode
import Shape
import Utils
import Vec

data Location = Location [Motion] Shape deriving (Show)

instance Collidable Location where
  cast (Location ms s) r = foldl (\a m -> map (unapply m) a) (cast s (foldr apply r ms)) ms
  contains (Location ms s) v = contains s (foldr vapply (into v) ms)

data Motion = Translate Vec | Rotate Vec Double | Scale Double deriving (Show)

vapply :: Motion -> Vec -> Vec
vapply (Translate t) v = v .-. t
vapply (Rotate axis angle) v = rot axis v (- angle)
vapply (Scale s) v = v ./ s

apply :: Motion -> Ray -> Ray
apply t@(Translate _) (Ray p d) = Ray (vapply t p) d
apply r@(Rotate _ _) (Ray p d) = Ray (vapply r p) (vapply r d)
apply s@(Scale _) (Ray p d) = Ray (vapply s p) (vapply s d)

unapply :: Motion -> CNode -> CNode
unapply (Translate v) (CNode pos dir col d) = CNode (pos .+. v) dir col d
unapply (Rotate axis angle) (CNode pos dir col d) = CNode (rot axis pos angle) (rot axis dir angle) col d
unapply (Scale s) (CNode pos dir col d) = CNode (pos .* s) dir col d

location :: [Motion] -> Shape -> Shape
location ms s = Shape $ Location ms s
