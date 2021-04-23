module Main where

import Camera
import Render
import Shape
import Shape.Sphere
import Vec

testCanvas =
  unlines . map (concatMap toString) $
    canvas (lookat e0 ex) (40, 20) (View 1 1) [sphere (0.25 *. ex) 0.1 red]

main :: IO ()
main = putStrLn testCanvas
