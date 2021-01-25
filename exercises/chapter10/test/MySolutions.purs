module Test.MySolutions where

import Prelude
import Data.Argonaut (Json, JsonDecodeError, decodeJson, encodeJson)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3)
import Data.Map
import Data.Pair (Pair(..))
import Data.Set
import Test.Examples

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
