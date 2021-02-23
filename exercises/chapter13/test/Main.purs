module Test.Main where

import Prelude
import Data.Array (foldr, range, reverse, sort, sortBy)
import Data.Char (fromCharCode)
import Data.Foldable (all, foldr)
import Data.Function (on)
import Data.List (List(..), foldMap, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..))
import Data.NonEmpty (NonEmpty(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Merge (mergeWith, mergePoly, merge)
import Sorted (sorted)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, Result(..), arbitrary, coarbitrary, quickCheck, quickCheckPure, (<?>))
import Test.QuickCheck.Gen (Gen, elements, arrayOf, oneOf)
import Tree (Tree, anywhere, fromArray, insert, member, toArray)

isSorted :: forall a. (Ord a) => Array a -> Boolean
isSorted = go <<< fromFoldable
  where
  go (Cons x1 t@(Cons x2 _)) = x1 <= x2 && go t

  go _ = true

ints :: Array Int -> Array Int
ints = identity

bools :: Array Boolean -> Array Boolean
bools = identity

intToBool :: (Int -> Boolean) -> Int -> Boolean
intToBool = identity

treeOfInt :: Tree Int -> Tree Int
treeOfInt = identity

main :: Effect Unit
main = do
  -- Tests for module 'Merge'
  quickCheck \xs ys ->
    let
      result = merge (sort xs) (sort ys)

      expected = sort $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show expected
  quickCheck \xs ys ->
    eq (merge (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)
  quickCheck \xs ys ->
    eq (bools $ mergePoly (sorted xs) (sorted ys)) (sort $ sorted xs <> sorted ys)
  quickCheck \xs ys f ->
    let
      result = map f $ mergeWith (intToBool f) (sortBy (compare `on` f) xs) (sortBy (compare `on` f) ys)

      expected = map f $ sortBy (compare `on` f) $ xs <> ys
    in
      eq result expected <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show xs
  quickCheck \xs ->
    let
      result = (merge xs [])
    in
      eq result xs <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show xs
  quickCheck \xs ->
    let
      result = reverse xs
    in
      eq (ints $ reverse result) xs <?> "Result:\n" <> show result <> "\nnot equal to expected:\n" <> show xs
  quickCheck \xs ys zs f ->
    let
      xss = sortBy (compare `on` f) xs

      yss = sortBy (compare `on` f) ys

      zss = sortBy (compare `on` f) zs

      xythenz = mergeWith (intToBool f) (mergeWith (intToBool f) xss yss) zss

      xthenyz = mergeWith (intToBool f) xss (mergeWith (intToBool f) yss zss)
    in
      eq xythenz xthenyz
  -- Tests for module 'Tree'
  quickCheck \t a -> member a $ insert a $ treeOfInt t
  quickCheck \t xs -> isSorted $ toArray $ foldr insert t $ ints xs
  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t
      == anywhere f (treeOfInt t)
      || anywhere g t
  quickCheck \t a xs ->
    let
      firstTree = insert a $ treeOfInt t

      addedTree = foldr insert firstTree (xs :: Array Int)
    in
      member a firstTree && member a addedTree
  quickCheck \f g t ->
    anywhere (\s -> f s || g s) t == anywhere f (treeOfInt t) || anywhere g t

newtype AZString
  = AZString String

instance showAZString :: Show AZString where
  show (AZString s) = s

instance arbAZString :: Arbitrary AZString where
  arbitrary = map (AZString <<< fromCharArray) $ arrayOf (elements $ NonEmpty 'a' lowercase)
    where
    lowercase = case traverse fromCharCode (range 98 122) of
      Just lc' -> lc'
      Nothing -> []

newtype Byte
  = Byte Int

instance arbitraryByte :: Arbitrary Byte where
  arbitrary = map intToByte arbitrary
    where
    intToByte n
      | n >= 0 = Byte (n `mod` 256)
      | otherwise = intToByte (-n)

instance coarbByte :: Coarbitrary Byte where
  coarbitrary b = coarbitrary b

data OneTwoThree a
  = One a
  | Two a a
  | Three a a a

instance arbitraryOneTwoThree :: Arbitrary a => Arbitrary (OneTwoThree a) where
  arbitrary =
    oneOf
      $ NonEmpty (map One arbitrary)
          [ Two <$> arbitrary <*> arbitrary
          , Three <$> arbitrary <*> arbitrary <*> arbitrary
          ]

instance coarbOneTwoThree :: Coarbitrary a => Coarbitrary (OneTwoThree a) where
  coarbitrary (One x) = coarbitrary x
  coarbitrary (Two x y) = coarbitrary x <<< coarbitrary y
  coarbitrary (Three x y z) = coarbitrary x <<< coarbitrary y <<< coarbitrary z

booleanQCP :: List Result -> Boolean
booleanQCP = all (\x -> case x of
                     Success -> true
                     Failed _ -> false)

simplifyQCP :: List Result -> First String
simplifyQCP rs = foldMap (\x -> case x of
                             Success -> First Nothing
                             Failed e -> First (Just e)) rs
