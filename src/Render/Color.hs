{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Render.Color where

import Control.Parallel.Strategies
import qualified Data.Text as T
import GHC.Generics (Generic)
import Render
import Vec

data Color = Color
  { _red :: Double,
    _green :: Double,
    _blue :: Double
  }
  deriving (Generic)

instance NFData Color

lux :: Color -> Double
lux (Color r g b) = norm (Vec r g b) / 3 ** (1 / 3)

instance Show Color where
  show c = [col !! id]
    where
      col = " .,=*;%#$@"
      len = length col
      id = min (len -1) (floor $ fromIntegral len * lux c)

-- Colors
red = Color 1 0 0

green = Color 0 1 0

blue = Color 0 0 1

yellow = Color 1 1 0

cyan = Color 0 1 1

majanta = Color 1 0 1

black = Color 0 0 0

white = Color 1 1 1

disp :: [[Color]] -> String
disp = unlines . map (concatMap show)

colorize :: Color -> T.Text
colorize (Color r g b) = "\ESC[48;5;" <> T.pack (show code) <> "m  "
  where
    code = 16 + foldl (\a i -> min 5 (floor (i * 6)) + 6 * a) 0 [r, g, b]

dispc :: [[Color]] -> T.Text
dispc = ("\ESC[2J" <>) . foldl (<>) "" . parMap rdeepseq ((<> "\ESC[0m\n") . foldl (<>) "" . map colorize)
