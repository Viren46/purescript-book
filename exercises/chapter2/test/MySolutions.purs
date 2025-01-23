module Test.MySolutions where

import Prelude
import Math (sqrt,pi)
import Data.Int (rem)

diagonal w h = sqrt (w * w + h * h)
circleArea r = pi * r * r

leftoverCents x = rem x 100