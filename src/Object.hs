{-# LANGUAGE ExistentialQuantification #-}

module Object where

import Vec

class Object_ a where
  position :: a -> Vec

data Object = forall a. (Object_ a) => Object a

instance Object_ Object where
  position (Object o) = position o

distance :: Object -> Object -> Double
distance a b = norm $ position a .-. position b
