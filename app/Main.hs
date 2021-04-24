module Main where

import Camera
import Render
import Render.Color
import Shape
import Shape.Collections
import Shape.Sphere
import Vec

main :: IO ()
main = do
  let frame = canvas (lookat e0 ex) (40, 20) (View 1 1) [sphere (10 *. ex) 1 red]
  putStrLn $ unlines . map (concatMap show) $ frame
