module Test.MySolutions where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromString, jsonParser, stringify)
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson)
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Pair (Pair(..))
import Data.Set (Set)
import Test.Examples

-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

-- type Complex = {
--   real :: Number,
--   imag :: Number
-- }
-- foreign import addComplex :: Complex -> Complex -> Complex
foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsImpl :: (forall x. x -> x -> Pair x) -> Quadratic -> Pair Complex

foreign import negateComplex :: Complex -> Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsImpl Pair q

foreign import valuesOfMapImpl :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = decodeJson <<< valuesOfMapImpl <<< encodeJson

valuesOfMapGeneric :: forall k v. EncodeJson k => Ord k => Ord v => DecodeJson v => EncodeJson v => Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = decodeJson <<< valuesOfMapImpl <<< encodeJson

foreign import quadraticRootsSetImpl :: Json -> Json

quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetImpl >>> decodeJson

jsonPairFromArr :: forall a. Array a -> Maybe (JsonPair a)
jsonPairFromArr [ car, cdr ] = Just $ JsonPair $ Pair car cdr

jsonPairFromArr _ = Nothing

newtype JsonPair a
  = JsonPair (Pair a)

instance decodeJsonPair :: DecodeJson a => DecodeJson (JsonPair a) where
  decodeJson json = do
    arr <- decodeJson json
    case arr of
      [ car, cdr ] ->
        JsonPair
          <$> (Pair <$> (decodeJson car) <*> (decodeJson cdr))
      [] -> Left $ AtIndex 0 MissingValue
      [ _ ] -> Left $ AtIndex 1 MissingValue
      _ -> Left $ UnexpectedValue (fromMaybe (fromString "") $ arr !! 2)

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe =
  encodeJson
    >>> quadraticRootsSetImpl
    >>> decodeJson
    >>> map (\(JsonPair p) -> p)

parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D s = do
  j <- jsonParser s
  case decodeJson j of
    Left err -> Left $ show err
    Right arr -> Right arr

data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson tree = genericEncodeJson tree

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson tree = genericDecodeJson tree

instance showTree :: Show a => Show (Tree a) where
  show tree = genericShow tree

instance eqTree :: Eq a => Eq (Tree a) where
  eq tree1 tree2 = genericEq tree1 tree2

data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance genericIntOrString :: Generic IntOrString _

instance showIntOrString :: Show IntOrString where
  show = genericShow

instance eqIntOrString :: Eq IntOrString where
  eq = genericEq

instance encodeJsonIntOrString :: EncodeJson IntOrString where
  encodeJson (IntOrString_Int n) = encodeJson n
  encodeJson (IntOrString_String s) = encodeJson s

instance decodeJsonIntOrString :: DecodeJson IntOrString where
  decodeJson j =
    (IntOrString_Int <$> decodeJson j)
      <|> (IntOrString_String <$> decodeJson j)
