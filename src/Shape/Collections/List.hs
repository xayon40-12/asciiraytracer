{-# LANGUAGE FlexibleInstances #-}

module Shape.Collections.List where

import Shape
import Shape.Collections

instance Collidable [Shape] where
  cast ss ray = go ss
    where
      go [] = []
      go (Shape s : ss) = cast s ray ++ go ss
  contains ss v = foldr (\i a -> contains i v || a) False ss
