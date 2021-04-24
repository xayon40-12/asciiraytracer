module Main where

import Camera
import Data.Maybe
import Render
import qualified Render.CNode
import Render.Color
import Shape
import Shape.Collections
import Shape.Collections.Intersection
import Shape.Collections.Location
import Shape.Collections.Substraction
import Shape.Collections.Union
import Shape.Sphere
import Vec

main :: IO ()
main = do
  let d = 3
  let s = sphere e0 1 (Color 0.1 0.1 0.1)
  let s2 = sphere ez 1 (Color 0.5 0.5 0.5)
  let s3 = sphere ey 1 (Color 1 1 1)
  let int = location [Scale 2, Translate (2 *. ey)] $ s <.> s2 <.> s3
  let uni = location [Scale 0.5, Rotate ey (3 * pi / 4), Translate ((-1) *. ey .+. 1 *. ez)] $ s >.< s2 >.< s3
  let sub = location [Scale 0.5, Rotate ey (- pi / 4), Translate ((-2) *. ez)] $ s <.< s2 <.< s3
  let o = [int, uni, sub]
  let frame = canvas (lookat (10 *. ex) ex) (120, 60) (View 1 1) o
  putStrLn $ disp frame
