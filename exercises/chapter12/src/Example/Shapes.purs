module Example.Shapes where

import Prelude

import Data.Array ((..), head)
import Data.Enum (enumFromThenTo)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Graphics.Canvas (Context2D(..), arc, closePath, fillPath, getCanvasElementById, getContext2D, lineTo, moveTo, rect, setFillStyle, setLineWidth, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate ::
  forall r.
  Number ->
  Number ->
  { x :: Number, y :: Number | r } ->
  { x :: Number, y :: Number | r }
translate dx dy shape =
  shape
    { x = shape.x + dx
    , y = shape.y + dy
    }

main :: Effect Unit
main =
  void
    $ unsafePartial do
        Just canvas <- getCanvasElementById "canvas"
        ctx <- getContext2D canvas
        setFillStyle ctx "#00F"
        fillPath ctx
          $ do
              rect ctx
                $ translate (-200.0) (-200.0)
                    { x: 250.0
                    , y: 250.0
                    , width: 100.0
                    , height: 100.0
                    }
              rect ctx
                $ translate (-50.0) (-200.0)
                    { x: 250.0
                    , y: 250.0
                    , width: 100.0
                    , height: 100.0
                    }
        setFillStyle ctx "#0F0"
        setStrokeStyle ctx "black"
        strokePath ctx $ arc ctx
          $ translate 200.0 200.0
              { x: 300.0
              , y: 300.0
              , radius: 50.0
              , start: 0.0
              , end: Math.tau * 2.0 / 3.0
              }
        fillPath ctx
          $ do
              arc ctx
                $ { x: 200.0
                  , y: 200.0
                  , radius: 150.0
                  , start: Math.tau * 1.0 / 12.0
                  , end: Math.tau * 3.0 / 12.0
                  }
              lineTo ctx 200.0 200.0
              closePath ctx
        setFillStyle ctx "#F00"
        setLineWidth ctx 2.0
        strokePath ctx
          $ do
              moveTo ctx 300.0 260.0
              lineTo ctx 260.0 340.0
              lineTo ctx 340.0 340.0
              closePath ctx
        fillPath ctx
          $ do
              moveTo ctx 300.0 260.0
              lineTo ctx 260.0 340.0
              lineTo ctx 340.0 340.0
              closePath ctx
        renderPath ctx
          (map (translate 100.0 500.0)
          (map f (map ((_ / 10.0) <<< toNumber) ((-100) .. 100))))
    where
      f :: Number -> Point
      f k = {x: 10.0 * k, y: -10.0 * k * k}

type Point
  = { x :: Number, y :: Number }

renderPath :: Context2D -> Array Point -> Effect Unit
renderPath ctx ps = do
  setStrokeStyle ctx "black"
  case head ps of
    Nothing -> pure unit
    Just h -> strokePath ctx $ do
      moveTo ctx h.x h.y
      _ <- traverse (\p -> lineTo ctx p.x p.y) ps
      pure unit
