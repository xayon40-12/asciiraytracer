{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shape where

import Render
import Render.CNode
import Utils
import Vec

class Collidable s where
  cast :: s -> Ray -> [CNode]
  contains :: (Into Vec v) => s -> v -> Bool

data Shape = forall a. (Collidable a, Show a) => Shape a

deriving instance Show Shape

instance Collidable Shape where
  cast (Shape s) = cast s
  contains (Shape s) = contains s
