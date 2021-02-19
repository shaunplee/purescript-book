module Example.RandomCircle where

import Prelude
import Data.Foldable (for_)
import Data.Int (hexadecimal, toNumber, toStringAs)
import Data.Maybe (Maybe(Just))
import Effect (Effect)
import Effect.Console (logShow)
import Effect.Random (random, randomInt)
import Graphics.Canvas (Context2D, arc, fillPath, getCanvasElementById, getContext2D, restore, save, setFillStyle, rotate, translate)
import Math (tau)
import Partial.Unsafe (unsafePartial)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (toEventTarget)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, getBoundingClientRect)
import Web.HTML.Window (document)
import Web.UIEvent.MouseEvent (clientX, clientY, fromEvent)

main :: Effect Unit
main =
  void
    $ unsafePartial do
        Just canvas <- getCanvasElementById "canvas"
        ctx <- getContext2D canvas
        curWin <- window
        doc <- document curWin
        Just node <- querySelector (QuerySelector "#canvas") $ toParentNode $ toDocument doc
        let
          (Just htmlNode) = fromElement node
        save ctx
        onClick <-
          eventListener
            ( \e -> do
                rect <- getBoundingClientRect (htmlNode)
                let
                  Just me = fromEvent e
                let
                  xpos = (toNumber $ clientX me) - rect.left
                let
                  ypos = (toNumber $ clientY me) - rect.top
                logShow $ "Mouse clicked at (" <> show xpos <> "," <> show ypos <> ")"
                rotateScene ctx xpos ypos
                render ctx xpos ypos
            )
        addEventListener (EventType "click") onClick true $ toEventTarget node

render :: Context2D -> Number -> Number -> Effect Unit
render ctx xpos ypos = do
  rad <- map (_ * 200.0) random
  color <- map (toStringAs hexadecimal) (randomInt 0 4095)
  restore ctx
  setFillStyle ctx ("#" <> color)
  fillPath ctx
    $ arc ctx
        { x: xpos
        , y: ypos
        , radius: rad
        , start: 0.0
        , end: tau
        }

rotateScene :: Context2D -> Number -> Number -> Effect Unit
rotateScene ctx xpos ypos = do
  translate ctx { translateX: xpos, translateY: ypos }
  rotate ctx (0.25 * tau)
  translate ctx { translateX: -xpos, translateY: -ypos }
