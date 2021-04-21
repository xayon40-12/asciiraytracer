module Shape where

import Vec

type Distance = Double

-- Ray Pos Dir
data Ray = Ray Vec Vec

-- CNode Pos Dir Distance
data CNode = CNode Vec Vec Distance

type Collisions = [CNode]

class Shape s where
  cast :: s -> Ray -> Collisions
