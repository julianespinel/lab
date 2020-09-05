module Euler where

import Prelude
import Data.List (range, filter)
import Data.Foldable (sum)
import Math (sqrt, pi, e)
import Global (readFloat)

ns n = range 0 (n - 1)

multiples n = filter (\n -> mod n 3 == 0 || mod n 5 == 0) (ns n)

answer n = sum (multiples n)

diagonal :: Number -> Number -> Number
diagonal x y = sqrt $ (x*x) + (y*y)

circleArea :: Number -> Number
circleArea r = pi * r * r

addE :: String -> Number
addE str = (readFloat str) + e
