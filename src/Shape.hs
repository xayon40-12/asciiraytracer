{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shape where

import Render
import Vec

type Distance = Double

-- Ray Pos Dir
data Ray = Ray Vec Vec deriving (Show)

-- CNode Pos Dir Distance
data CNode = CNode Vec Vec Distance deriving (Show)

instance Drawable CNode where
  toString (CNode _ _ d) = [col !! id]
    where
      col = " .,:=;#@"
      len = length col
      id = min (len -1) (floor $ 1 / d)

class Collidable s where
  cast :: s -> Ray -> Maybe CNode

data Shape = forall a. (Collidable a, Show a) => Shape a

deriving instance Show Shape

instance Collidable Shape where
  cast (Shape s) = cast s
