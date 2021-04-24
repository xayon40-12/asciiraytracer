module Light where

import Render.Color
import Shape
import Vec

data Light = Light
  { _pos :: Vec,
    _color :: Color,
    _lux :: Double
  }
