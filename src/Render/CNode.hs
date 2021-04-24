{-# LANGUAGE MultiParamTypeClasses #-}

module Render.CNode where

import Render
import Render.Color
import Utils
import Vec

data CNode = CNode
  { _pos :: Vec,
    _dir :: Vec,
    _color :: Color,
    _distance :: Distance
  }
  deriving (Show)

instance Into Vec CNode where
  into (CNode p _ _ _) = p
