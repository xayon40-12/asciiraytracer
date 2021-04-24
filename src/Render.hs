{-# LANGUAGE DuplicateRecordFields #-}

module Render where

import Vec

type Distance = Double

data Ray = Ray
  { _pos :: Vec,
    _dir :: Vec
  }
  deriving (Show)
