{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shape where

import Object
import Render
import Vec

class Collidable s where
  cast :: s -> Ray -> Maybe CNode
  contains :: s -> Vec -> Bool

data Shape = forall a. (Collidable a, Show a, Object_ a) => Shape a

deriving instance Show Shape

instance Collidable Shape where
  cast (Shape s) = cast s
  contains (Shape s) = contains s

instance Object_ Shape where
  position (Shape s) = position s

-- Colors
red = Color 1 0 0

green = Color 0 1 0

blue = Color 0 0 1

yellow = Color 1 1 0

cyan = Color 0 1 1

majanta = Color 1 0 1

black = Color 0 0 0

white = Color 1 1 1
