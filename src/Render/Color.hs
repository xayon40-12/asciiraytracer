module Render.Color where

import Render
import Vec

data Color = Color
  { _red :: Double,
    _green :: Double,
    _blue :: Double
  }

lux :: Color -> Double
lux (Color r g b) = norm (Vec r g b) / 3 ** (1 / 3)

instance Show Color where
  show c = [col !! id]
    where
      col = " .,=*;%#$@"
      len = length col
      id = min (len -1) (floor $ fromIntegral len * lux c)

-- Colors
red = Color 1 0 0

green = Color 0 1 0

blue = Color 0 0 1

yellow = Color 1 1 0

cyan = Color 0 1 1

majanta = Color 1 0 1

black = Color 0 0 0

white = Color 1 1 1
