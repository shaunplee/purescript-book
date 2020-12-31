module Test.MySolutions where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex { real, imaginary }) =
    show real
      <> (if imaginary < 0.0 then "" else "+")
      <> show imaginary
      <> "i"

derive newtype instance eqComplex :: Eq Complex

derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  zero = Complex { real: 0.0, imaginary: 0.0 }
  add =
    over2 wrap
      ( \({ real: r1, imaginary: i1 }) ({ real: r2, imaginary: i2 }) ->
          { real: r1 + r2, imaginary: i1 + i2 }
      )
  one = Complex { real: 1.0, imaginary: 1.0 }
  mul =
    over2 wrap
      ( \({ real: r1, imaginary: i1 }) ({ real: r2, imaginary: i2 }) ->
          { real: r1 * r2 - i1 * i2, imaginary: r1 * i2 + r2 * i1 }
      )

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow
