module Data.DOM.Phantom
  ( Element
  , Attribute
  , Content
  , AttributeKey
  , PixelsPercent
  , False
  , True
  , class IsValue
  , toValue
  , class IsEmptyAttrib
  , isEmpty

  , a
  , p
  , img

  , href
  , _class
  , src
  , width
  , height
  , pixels
  , percent

  , attribute, (:=)
  , text
  , elem

  , render
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: Array (Attribute)
  , content      :: Maybe (Array Content)
  }

data Content
  = TextContent String
  | ElementContent Element

data PixelsPercent = Pixels Int
                   | Percent Int

data True

data False

newtype Attribute = Attribute
  { key          :: String
  , value        :: String
  }

newtype AttributeKey a b = AttributeKey String

element :: String -> Array Attribute -> Maybe (Array Content) -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

pixels :: Int -> PixelsPercent
pixels = Pixels

percent :: Int -> PixelsPercent
percent = Percent

class IsValue a where
  toValue :: a -> String

instance stringIsValue :: IsValue String where
  toValue = identity

instance intIsValue :: IsValue Int where
  toValue = show

instance pixelsPercentIsValue :: IsValue (PixelsPercent) where
  toValue (Pixels n) = show n <> "px"
  toValue (Percent n) = show n <> "%"

class IsEmptyAttrib a where
  isEmpty :: a -> Boolean

instance isEmptyAttribTrue :: IsEmptyAttrib True where
  isEmpty _ = true

instance isEmptAttribFalse :: IsEmptyAttrib False where
  isEmpty _ = false

attribute :: forall a b. IsEmptyAttrib b => IsValue a => AttributeKey a b -> a -> Attribute
attribute (AttributeKey key) value = Attribute
  { key: key
  , value: toValue value
  }

infix 4 attribute as :=

a :: Array Attribute -> Array Content -> Element
a attribs content = element "a" attribs (Just content)

p :: Array Attribute -> Array Content -> Element
p attribs content = element "p" attribs (Just content)

img :: Array Attribute -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey False String
href = AttributeKey "href"

_class :: AttributeKey False String
_class = AttributeKey "class"

src :: AttributeKey False String
src = AttributeKey "src"

width :: AttributeKey False PixelsPercent
width = AttributeKey "width"

height :: AttributeKey False PixelsPercent
height = AttributeKey "height"

disabled :: AttributeKey True String
disabled = AttributeKey "disabled"

render :: Element -> String
render (Element e) =
    "<" <> e.name <>
    " " <> joinWith " " (map renderAttribute e.attribs) <>
    renderContent e.content
  where
    renderAttribute :: Attribute -> String
    renderAttribute (Attribute x) = x.key <> "=\"" <> x.value <> "\""

    renderContent :: Maybe (Array Content) -> String
    renderContent Nothing = " />"
    renderContent (Just content) =
        ">" <> joinWith "" (map renderContentItem content) <>
        "</" <> e.name <> ">"
      where
        renderContentItem :: Content -> String
        renderContentItem (TextContent s) = s
        renderContentItem (ElementContent e') = render e'
