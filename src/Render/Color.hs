module Render.Color where

import Render
import Vec

data Color = Color
  { _red :: Double,
    _green :: Double,
    _blue :: Double
  }
  deriving (Show)

lux :: Color -> Double
lux (Color r g b) = norm (Vec r g b) / 3 ** (1 / 3)

instance Drawable Color where
  toString c = [col !! id]
    where
      col = " .,=*;%#$@"
      len = length col
      id = min (len -1) (floor $ fromIntegral len * lux c)
