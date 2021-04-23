module Render.CNode where

import Render
import Vec

data CNode = CNode
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color,
    _distance :: Distance
  }
  deriving (Show)

instance Drawable CNode where
  toString _ = "#"
