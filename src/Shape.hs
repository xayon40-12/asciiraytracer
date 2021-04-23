{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}

module Shape where

import Render
import Vec

type Distance = Double

-- Ray Pos Dir
data Ray = Ray Vec Vec deriving (Show)

-- Color Red Green Blue
data Color = Color Double Double Double deriving (Show)

lux :: Color -> Double
lux (Color r g b) = norm (Vec r g b) / 3 ** (1 / 3)

instance Drawable Color where
  toString c = [col !! id]
    where
      col = " .,=*;%#$@"
      len = length col
      id = min (len -1) (floor $ fromIntegral len * lux c)

-- CNode Pos Dir Distance
data CNode = CNode
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color,
    _distance :: Distance
  }
  deriving (Show)

instance Drawable CNode where
  toString _ = "#"

class Collidable s where
  cast :: s -> Ray -> Maybe CNode

data Shape = forall a. (Collidable a, Show a) => Shape a

deriving instance Show Shape

instance Collidable Shape where
  cast (Shape s) = cast s

-- Colors
red = Color 1 0 0

green = Color 0 1 0

blue = Color 0 0 1

yellow = Color 1 1 0

cyan = Color 0 1 1

majanta = Color 1 0 1

black = Color 0 0 0

white = Color 1 1 1
