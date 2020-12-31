module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Math (pi, sqrt)

diagonal w h = sqrt (w * w + h * h)

circleArea r = pi * r * r

leftoverCents v = v `rem` 100
