module Test.MySolutions where

import Prelude
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

newtype Foo
  = MakeFoo Int

derive instance newtypeFoo :: Newtype Foo _

derive newtype instance showFoo :: Show Foo

unFoo :: Foo -> Int
unFoo = unwrap

incFoo :: Foo -> Foo
incFoo x = over wrap ((+) 1) x

addFoo :: Foo -> Foo -> Foo
addFoo x y = over2 wrap (+) x y

addComplex :: Complex -> Complex -> Complex
addComplex x y = over2 wrap (\({ real: r1, imaginary: i1 }) ({ real: r2, imaginary: i2 }) -> { real: r1 + r2, imaginary: i1 + i2 }) x y
