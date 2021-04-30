module Shape.Collections.Union where

import Render.CNode
import Shape
import Shape.Collections
import Utils
import Vec

data Union = Union Shape Shape deriving (Show)

instance Collidable Union where
  cast i@(Union a b) r = cn1 ++ cn2
    where
      cn1 = filter (not . contains b) (cast a r)
      cn2 = filter (not . contains a) (cast b r)
  contains (Union a b) v = contains a v && contains b v

union :: Shape -> Shape -> Shape
union a b = Shape $ Union a b

infixr 6 >.<

(>.<) :: Shape -> Shape -> Shape
(>.<) = union
