module Shape.Collections.Intersection where

import Render.CNode
import Shape
import Shape.Collections
import Utils
import Vec

data Intersection = Intersection Shape Shape deriving (Show)

instance Collidable Intersection where
  cast i@(Intersection a b) r = cn1 ++ cn2
    where
      cn1 = filter (contains b) (cast a r)
      cn2 = filter (contains a) (cast b r)
  contains (Intersection a b) v = contains a v && contains b v

intersection :: Shape -> Shape -> Shape
intersection a b = Shape $ Intersection a b

infixr 7 <.>

(<.>) :: Shape -> Shape -> Shape
(<.>) = intersection
