{-# LANGUAGE FlexibleInstances #-}

module Shape.Collections.List where

import Shape
import Shape.Collections

instance Collidable [Shape] where
  cast ss ray = go ss
    where
      go [] = Nothing
      go (Shape s : ss) = nearest (cast s ray) (go ss)
