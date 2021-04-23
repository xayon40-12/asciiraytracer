module Light where

import Shape
import Vec

data Light = Light
  { _pos :: Vec,
    _color :: Color,
    _lux :: Double
  }
