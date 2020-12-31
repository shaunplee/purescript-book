module Test.MySolutions where

import Data.Foldable
import Data.Path
import Data.String.Pattern
import Prelude
import Control.MonadZero (guard)
import Data.Array (concatMap, filter, head, last, length, null, snoc, tail, (..), (:))
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.String (split)

-- import Data.Traversable (maximumBy, minimumBy, traverse)
-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven i =
  if i == 0 then
    true
  else
    if i == 1 then
      false
    else
      if i < 0 then
        isEven (i + 2)
      else
        isEven (i - 2)

countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else
    if isEven $ fromMaybe 0 $ head arr then
      1 + countEven (fromMaybe [] $ tail arr)
    else
      countEven (fromMaybe [] $ tail arr)

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter ((<=) 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite arr = ((<=) 0.0) <$?> arr

myFactors :: Int -> Array (Array Int)
myFactors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [ i, j ]

isPrime :: Int -> Boolean
isPrime x =
  if x == 1 then
    false
  else
    (==) 1 <<< length <<< myFactors $ x

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- 1 .. a
  c <- 1 .. b
  guard $ (c * c + b * b == a * a)
  pure [ c, b, a ]

factorize :: Int -> Array Int
factorize n =
  if n == 1 then
    [ 1 ]
  else
    let
      factors = myFactors n
    in
      if length factors == 1 then
        [ fromMaybe 0 $ second =<< head factors ]
      else
        let
          fzn = fromMaybe [] $ second factors

          x = fromMaybe 0 $ head fzn

          y = fromMaybe 0 $ second fzn
        in
          snoc (factorize y) $ x

second :: forall a. Array a -> Maybe a
second xs = do
  t <- tail xs
  head t

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec 0 = 1

fibTailRec n = go 1 1 0
  where
  go c n1 n2 = if n == c then n1 + n2 else go (c + 1) (n1 + n2) n1

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [ x ] <> xs) []

myAllFiles :: Path -> Array Path
myAllFiles file = file : concatMap myAllFiles (ls file)

myAllFiles' :: Path -> Array Path
myAllFiles' file =
  file
    : do
        child <- ls file
        myAllFiles' child

onlyFiles :: Path -> Array Path
onlyFiles p =
  if isDirectory p then
    concatMap onlyFiles (ls p)
  else
    [ p ]

whereIs :: Path -> String -> Maybe Path
whereIs p fn = do
  let
    fs = onlyFiles p

    fns = filter (\f -> (==) (pathToFileName f) fn) fs
  if null fns then
    Nothing
  else do
    let
      af = myAllFiles' p

      ds = filter isDirectory af

      tfn = filename $ fromMaybe root (head fns)
    head $ filter (\d -> not null $ filter ((==) tfn) (map filename $ ls d)) ds

pathToFileName :: Path -> String
pathToFileName = fromMaybe "" <<< last <<< split (Pattern "/") <<< filename

largestSmallest :: Path -> Array Path
largestSmallest p =
  let
    af = myAllFiles' p

    saf = filter (\x -> isJust $ size x) af

    maxP = maximumBy (\x y -> compare (fromMaybe 0 $ size x) (fromMaybe 0 $ size y)) saf

    minP = minimumBy (\x y -> compare (fromMaybe 0 $ size x) (fromMaybe 0 $ size y)) saf
  in
    if isJust maxP && isJust minP then
      let
        maxPm = fromMaybe root maxP

        minPm = fromMaybe root minP
      in
        if filename maxPm == filename minPm then
          [ minPm ]
        else
          [ maxPm, minPm ]
    else
      if isJust maxP then
        [ fromMaybe root maxP ]
      else
        if isJust minP then
          [ fromMaybe root minP ]
        else
          []
