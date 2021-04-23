module Light where

import Object
import Shape
import Vec

data Light = Light
  { _pos :: Vec,
    _color :: Color,
    _lux :: Double
  }

instance Object_ Light where
  position l = Light._pos l
