module Main where

import Prelude

import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Section.BlockingGetN as BlockingGetN
import Section.CollectEventToAff as CollectEventToAff
import Section.Combine as Combine
import Util.Util (flexCol, styled)

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  D.div (flexCol)
    [ section "BlockingGetN", BlockingGetN.nut
    , section "CollectEventToAff", CollectEventToAff.nut
    , section "Combine", Combine.nut
    ]
  where
  section header = D.div (styled "margin-top: 10px; border-top: 1px solid gray") [D.h2_ [text_ header]]

main :: Effect Unit
main = runInBody (nut)
