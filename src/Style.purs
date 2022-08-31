module Style where

import Prelude

import Control.Alt ((<|>))
import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, (:=))
import Deku.Control (text_)
import Deku.Core (Nut)
import Deku.DOM (Class, Input_)
import Deku.DOM as D
import Deku.Listeners (click)
import Effect (Effect)
import FRP.Event (ZoraEvent)

inputCss :: String
inputCss = css "placeholder:text-gray-400 text-white font-semibold bg-gray-900 rounded border-none"

inputText :: ZoraEvent (Attribute Input_) -> Nut
inputText attrEv = D.input ((pure $ D.Xtype := "text") <|> attrEv) []

-- / Creates a Class event from a String
bangCss :: ∀ e .Attr e Class String => String -> ZoraEvent (Attribute e)
bangCss s = pure (D.Class := s)

btn :: String -> String -> ZoraEvent (Effect Unit) -> Nut
btn text extraCss onClick =
  D.button ((click onClick) <|> bangCss (baseCss <> grayCss <> extraCss)) [text_ text]

grayCss :: String
grayCss = css "bg-gray-500 hover:bg-gray-400"

baseCss :: String
baseCss = joinWith ""
  [ css "font-semibold px-3 py-1 text-white text-center"
  , css "rounded-md shadow-md cursor-pointer whitespace-nowrap"
  , css "leading-loose transition select-none touch-manipulation align-middle hover:no-underline "
  , css "active:shadow focus:outline-1 focus:outline-transparent before:hidden break-words "
  ]

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
css :: String -> String
css s = " " <> s <> " "
