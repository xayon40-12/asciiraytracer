{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}

module Camera where

import Data.Function ((&))
import Render
import Shape
import Shape.Collections.List
import Shape.Sphere
import Vec

data Camera = Camera
  { _pos :: Vec,
    _x :: Vec,
    _y :: Vec
  }

lookat :: Vec -> Vec -> Camera
lookat pos distantPos = Camera pos (normalize $ distantPos .-. pos) (Vec 0 1 0)

move :: Camera -> Double -> Camera
move cam@(Camera pos x _) distance = cam {_pos = pos .+. distance *. x}

rotate :: Camera -> Double -> Double -> Camera
rotate cam@(Camera p x y) phi theta = cam {_x = normalize $ rot x' z theta, _y = normalize $ rot y' z theta}
  where
    x' = rot x ey phi
    y' = rot y ey phi
    z = x' .^. y'

data View = View
  { _width :: Double,
    _Height :: Double
  }

type Size = (Int, Int)

canvas :: (Collidable c) => Camera -> Size -> View -> c -> [[Color]]
canvas (Camera cp cx cy) (nx, ny) (View w h) obj = res
  where
    cz = cx .^. cy
    xs = [1 .. nx] & map (\x -> w * ((fromIntegral x -0.5) / fromIntegral nx - 0.5))
    ys = [1 .. ny] & map (\y -> h * ((fromIntegral y -0.5) / fromIntegral ny - 0.5))
    ray x y = Ray cp (normalize $ cx .+. y *. cy .+. x *. cz)
    res = ys & map (\y -> xs & map (\x -> maybe black _color $ cast obj (ray x y)))
