module Main where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable, bus)
import Deku.DOM as D
import Deku.Do as Doku
import Deku.Listeners (click)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (AnEvent, keepLatest, memoize)
import Section.BlockingGetN as BlockingGetN
import Section.CollectEventToAff as CollectEventToAff
import Util.Util (flexCol, styled)

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  D.div (flexCol)
    [ section "BlockingGetN", BlockingGetN.nut
    , section "CollectEventToAff", CollectEventToAff.nut
    ]
  where
  section header = D.div (styled "margin-top: 10px; border-top: 1px solid gray") [D.h2_ [text_ header]]

main :: Effect Unit
main = runInBody (nut)
