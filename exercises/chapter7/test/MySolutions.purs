module Test.MySolutions where

import Prelude
import Control.Apply (class Apply, lift2)
import Data.AddressBook
import Data.AddressBook.Validation
import Data.Either
import Data.EuclideanRing (class EuclideanRing)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Functor (class Functor)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe
import Data.Ring (class Ring)
import Data.Semiring (class Semiring)
import Data.String.Regex (Regex, test, regex)
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file
addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 add

subMaybe :: forall a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = lift2 sub

mulMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = lift2 mul

divMaybe :: forall a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = lift2 div

addApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
addApply = lift2 add

subApply :: forall f a. Apply f => Ring a => f a -> f a -> f a
subApply = lift2 sub

mulApply :: forall f a. Apply f => Semiring a => f a -> f a -> f a
mulApply = lift2 mul

divApply :: forall f a. Apply f => EuclideanRing a => f a -> f a -> f a
divApply = lift2 div

combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing

combineMaybe (Just fa) = ado
  x <- fa
  in Just x

stateRegex :: Either String Regex
stateRegex = regex "^[A-z]{2}$" noFlags

nonEmptyRegex :: Either String Regex
nonEmptyRegex = regex "^.*[\\S].*$" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
    <*> matches "City" nonEmptyRegex a.city
    <*> matches "State" stateRegex a.state

data Tree a
  = Leaf
  | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
  show x = genericShow x

instance fuctorTree :: Functor Tree where
  map _ Leaf = Leaf
  map f (Branch tl x tr) = Branch (map f tl) (f x) (map f tr)

instance foldableTree :: Foldable Tree where
  foldl fba b t = case t of
    Leaf -> b
    (Branch tl x tr) -> foldl fba (fba (foldl fba b tl) x) tr
  foldr fab b t = case t of
    Leaf -> b
    (Branch tl x tr) -> foldr fab (fab x (foldr fab b tr)) tl
  foldMap _ Leaf = mempty
  foldMap f (Branch tl x tr) = foldMap f tl <> f x <> foldMap f tr

instance traversableTree :: Traversable Tree where
  traverse _ Leaf = pure Leaf
  traverse f (Branch tl x tr) = Branch <$> traverse f tl <*> f x <*> traverse f tr
  sequence Leaf = pure Leaf
  sequence (Branch tl x tr) = Branch <$> sequence tl <*> x <*> sequence tr

traversePreOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf

traversePreOrder f (Branch tl x tr) = ado
  n <- f x
  left <- traversePreOrder f tl
  right <- traversePreOrder f tr
  in Branch left n right

traversePostOrder :: forall a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf

traversePostOrder f (Branch tl x tr) = ado
  left <- traversePostOrder f tl
  right <- traversePostOrder f tr
  n <- f x
  in Branch left n right

type Person'
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

person' :: String -> String -> Maybe Address -> Array PhoneNumber -> Person'
person' firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: Person' -> V Errors Person'
validatePersonOptionalAddress p =
  person' <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: forall a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse tma =
  traverse
    ( \ma -> ado
        x <- ma
        in x
    )
    tma

traverseUsingSequence :: forall a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f ta = sequence $ map f ta
