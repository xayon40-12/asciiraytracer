{-# LANGUAGE BangPatterns #-}

module Main where

import Camera
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Function ((&))
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
import System.CPUTime
import Text.Printf
import Vec

diff s e = (fromIntegral (e - s) / 10 ^ 12) :: Double

main :: IO ()
main = do
  let x = ez
  let y = rot ex x (pi / 3)
  let n = 3
  let r = 0.5
  let ss = [0 .. n -1] & concatMap (\i -> [0 .. n -1 - i] & map (\j -> sphere (i *. x .+. j *. y) r (Color 1 0.2 0.3)))
  let st = sphere (((n -1) / 2 / sqrt 2) *. (x .+. y) .+. (n * r) *. ex) (n * r) (Color 0 1 1) >.> foldr (>.<) (head ss) (tail ss)
  let d = 3
  let s = sphere e0 1 (Color 1 0 0)
  let s2 = sphere ez 1 (Color 0 1 0)
  let s3 = sphere ey 1 (Color 0 0 1)
  let int = location [Scale 2, Translate (2 *. ey)] $ s <.> s2 <.> s3
  let uni = location [Scale 0.5, Rotate ey (3 * pi / 4), Translate ((-1) *. ey .+. 1 *. ez)] $ s >.< s2 >.< s3
  let sub = location [Scale 0.5, Rotate ey (- pi / 4), Translate ((-2) *. ez)] $ s <.< s2 <.< s3
  let o = [int, uni, sub, st]
  let f = 600
  forM_ [0 ..] $ \i -> do
    start <- getCPUTime
    let !can = canvas (lookat (rot ey (10 *. ex) (i / f * 2 * pi)) e0) (100, 100) (View 1 1) o
    ecan <- getCPUTime
    let !disp = dispc can
    edis <- getCPUTime
    B.putStrLn disp
    end <- getCPUTime
    printf "canvas: %0.3f s\ndispc: %0.3f s\nputStrLn: %0.3f s\n" (diff start ecan) (diff ecan edis) (diff edis end)
