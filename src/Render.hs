{-# LANGUAGE DuplicateRecordFields #-}

module Render where

import Vec

class Drawable d where
  toString :: d -> String

instance (Drawable d) => Drawable (Maybe d) where
  toString Nothing = " "
  toString (Just d) = toString d

type Distance = Double

data Ray = Ray
  { _pos :: Vec,
    _dir :: Vec
  }
  deriving (Show)
