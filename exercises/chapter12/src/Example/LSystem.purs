module Example.LSystem where

import Prelude

import Data.Array (concatMap, foldM)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, setFillStyle, setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

lsystem :: forall a m s
         . Monad m
         => Array a
         -> (a -> Array a)
         -> (s -> a -> m s)
         -> Int
         -> s
         -> m s
lsystem init prod interpret n state = go init n
  where
  go s 0 = foldM interpret state s
  go s i = go (concatMap prod s) (i - 1)

lsystemProduce :: forall a s . Array a -> (a -> Array a) -> Int -> Array a
lsystemProduce init prod n = go init n
  where
    go s 0 = s
    go s i = go (concatMap prod s) (i - 1)

lsystemInterpret :: forall a m s . Monad m => (s -> a -> m s) -> Array a -> s -> m s
lsystemInterpret interpret s state = foldM interpret state s

type Angle = Number

data Letter = L | R | F Boolean

type Sentence = Array Letter

type State =
  { x :: Number
  , y :: Number
  , theta :: Number
  }

turnL :: Number
turnL = Math.tau / 6.0
turnR :: Number
turnR = Math.tau / 6.0

initial :: Sentence
-- initial = [F, R turnR, R turnR, F, R turnR, R turnR, F, R turnR, R turnR]
initial = [F true]

productions :: Letter -> Sentence
productions L = [L]
productions R = [R]
--productions F = [F, L, F, R, R , F, L, F]
productions (F false) = [F false, L, F true, L, F false, R, F true, R, F false, R, F true, R, F false, L, F true, L, F false]
productions (F true) = [F true, R, F false, R, F true, L, F false, L, F true, L, F false, L, F true, R, F false, R, F true]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  let
    interpret :: State -> Letter -> Effect State
    interpret state L = pure $ state { theta = state.theta - turnL}
    interpret state R = pure $ state { theta = state.theta + turnR}
    interpret state _ = do
      let x = state.x + Math.cos state.theta * 1.50
          y = state.y + Math.sin state.theta * 1.50
      lineTo ctx x y
      pure { x, y, theta: state.theta }

  setStrokeStyle ctx "#000"
  setFillStyle ctx "#059"

  setShadowColor ctx "#A00"
  setShadowBlur ctx 5.0
  setShadowOffsetX ctx 10.0
  setShadowOffsetY ctx 15.0

  strokePath ctx $ do
    moveTo ctx initialState.x initialState.y
    let ls = lsystemProduce initial productions 4
    _ <- lsystemInterpret interpret ls initialState
    closePath ctx
