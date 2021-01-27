module Test.MySolutions where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromString, jsonParser, stringify)
import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn3)
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
