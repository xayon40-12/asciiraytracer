module Shape.Collections.Substraction where

import Render.CNode
import Shape
import Shape.Collections
import Utils
import Vec

data Substraction = Substraction Shape Shape deriving (Show)

instance Collidable Substraction where
  cast i@(Substraction a b) r = cn1 ++ cn2
    where
      cn1 = filter (not . contains b) (cast a r)
      cn2 = map (\(CNode pos dir col d) -> CNode pos (neg dir) col d) $ filter (contains a) (cast b r)
  contains (Substraction a b) v = contains a v && not (contains b v)

substraction :: Shape -> Shape -> Shape
substraction a b = Shape $ Substraction a b

infixr 7 >.>, <.<

(>.>) :: Shape -> Shape -> Shape
(>.>) = flip substraction

(<.<) :: Shape -> Shape -> Shape
(<.<) = substraction
