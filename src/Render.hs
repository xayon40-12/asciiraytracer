module Render where

class Drawable d where
  toString :: d -> String

instance (Drawable d) => Drawable (Maybe d) where
  toString Nothing = " "
  toString (Just d) = toString d
