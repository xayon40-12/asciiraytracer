module Vec where

data Vec = Vec
  { _x :: Double,
    _y :: Double,
    _z :: Double
  }

infixl 6 .+., .-.

infixl 7 .*., .*, *., ./, /., .^., .|.

(.+.) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) .+. (Vec x2 y2 z2) = Vec (x1 + x2) (y1 + y2) (z1 + z2)

(.-.) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) .-. (Vec x2 y2 z2) = Vec (x1 - x2) (y1 - y2) (z1 - z2)

(.*.) :: Vec -> Vec -> Double
(Vec x1 y1 z1) .*. (Vec x2 y2 z2) = (x1 * x2) + (y1 * y2) + (z1 * z2)

(.*) :: Vec -> Double -> Vec
(Vec x y z) .* d = Vec (x * d) (y * d) (z * d)

(*.) :: Double -> Vec -> Vec
d *. (Vec x y z) = Vec (x * d) (y * d) (z * d)

(./) :: Vec -> Double -> Vec
(Vec x y z) ./ d = Vec (x / d) (y / d) (z / d)

(/.) :: Double -> Vec -> Vec
d /. (Vec x y z) = Vec (x / d) (y / d) (z / d)

(.^.) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) .^. (Vec x2 y2 z2) = Vec (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

(.|.) :: Vec -> Vec -> Vec
(Vec x1 y1 z1) .|. (Vec x2 y2 z2) = Vec (x1 * x2) (y1 * y2) (z1 * z2)

norm :: Vec -> Double
norm v = sqrt (v .*. v)

norm2 :: Vec -> Double
norm2 v = v .*. v

normalize :: Vec -> Vec
normalize v = v ./ norm v

neg :: Vec -> Vec
neg (Vec x y z) = Vec (- x) (- y) (- z)

-- rot :: vec -> axis -> angle -> vec
rot :: Vec -> Vec -> Double -> Vec
rot v axis theta = c *. x .+. s *. y .+. z
  where
    z = n .* (n .*. v)
    x = v .-. z
    y = x .^. n
    n = normalize axis
    c = cos theta
    s = sin theta

mirror :: Vec -> Vec -> Vec
mirror v axis = 2 *. axis .* (axis .*. v) .-. v
