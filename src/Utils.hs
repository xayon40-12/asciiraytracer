{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Utils where

class Into a b where
  into :: b -> a

instance Into a a where
  into a = a
