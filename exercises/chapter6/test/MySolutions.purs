module Test.MySolutions where

import Prelude
import Data.Array (nubEq, nub)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (class Newtype, over, over2, unwrap, wrap)
import Data.Semigroup (class Semigroup)

-- Note to reader: Add your solutions to this file
newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance showPoint :: Show Point where
  show (Point { x, y }) = "(" <> show x <> ", " <> show y <> ")"

derive instance eqPoint :: Eq Point

derive instance ordPoint :: Ord Point

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

derive instance eqShape :: Eq Shape

derive instance ordShape :: Ord Shape

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a
  = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x == y) && (xs == ys)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [ y ] <> ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance showcNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty x xs) = "NonEmpty " <> show x <> " " <> show xs

data Extended a
  = Infinite
  | Finite a

instance eqExtended :: Eq a => Eq (Extended a) where
  eq Infinite Infinite = true
  eq Infinite _ = false
  eq _ Infinite = false
  eq (Finite x) (Finite y) = eq x y

instance ordExtended :: Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

instance foldableNonEmpty :: Foldable NonEmpty where
  foldl fba b (NonEmpty x xs) = foldl fba (fba b x) xs
  foldr fab b (NonEmpty x xs) = fab x (foldr fab b xs)
  foldMap fam (NonEmpty x xs) = fam x <> foldMap fam xs

data OneMore f a
  = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldl fba b (OneMore x xs) = foldl fba (fba b x) xs
  foldr fab b (OneMore x xs) = fab x (foldr fab b xs)
  foldMap fam (OneMore x xs) = fam x <> foldMap fam xs

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub
