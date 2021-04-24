module Render.CNode where

import Render
import Render.Color
import Vec

data CNode = CNode
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color,
    _distance :: Distance
  }
  deriving (Show)
