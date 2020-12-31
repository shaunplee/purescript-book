module Test.MySolutions where

import Prelude
import ChapterExamples
import Data.Maybe (Maybe(..))
import Data.Picture (Shape(..), origin)
import Math (pi)

factorial :: Int -> Int
factorial 0 = 1

factorial 1 = 1

factorial n
  | n < 0 = 0
  | otherwise = go n 1
    where
    go 1 acc = acc

    go k acc = go (k - 1) (acc * k)

binomial :: Int -> Int -> Int
binomial n k = factorial n / ((factorial k) * factorial (n - k))

pascal :: Int -> Int -> Int
pascal n k
  | k == 0 = 1
  | n == 0 = 0
  | n < k = 0
  | otherwise = pascal (n - 1) k + pascal (n - 1) (k - 1)

-- type Address
--   = { street :: String, city :: String }
-- type Person
--   = { name :: String, address :: Address }
sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [ x ] = x

fromSingleton d _ = d

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (2.0 * r)

doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (2.0 * w) (2.0 * h)

doubleScaleAndCenter (Line { x: x1, y: y1 } { x: x2, y: y2 }) = Line { x: nx1, y: ny1 } { x: nx2, y: ny2 }
  where
  xlen = x2 - x1

  ylen = y2 - y1

  nx1 = -xlen

  ny1 = -ylen

  nx2 = xlen

  ny2 = ylen

doubleScaleAndCenter (Text _ s) = Text origin s

doubleScaleAndCenter (Clipped p c w h) = Clipped (map doubleScaleAndCenter p) origin (2.0 * w) (2.0 * h)

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s

shapeText _ = Nothing

-- newtype Ohm
--   = Ohm Number
-- newtype Volt
--   = Volt Number
-- newtype Amp
--   = Amp Number
-- newtype Watt
--   = MakeWatt Number
calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = MakeWatt $ i * v

area :: Shape -> Number
area (Circle _ r) = pi * r * r

area (Rectangle _ w h) = w * h

area (Line _ _) = 0.0

area (Text _ _) = 0.0

area (Clipped _ c w h) = w * h
