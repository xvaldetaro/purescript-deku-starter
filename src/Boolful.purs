module Boolful where

import Prelude

import Bolson.Core (envy)
import Control.Alt ((<|>))
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Attribute ((:=))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable, bus)
import Deku.DOM as D
import Deku.Do as DekuDo
import Deku.Listeners (click)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import FRP.Event (AnEvent, keepLatest, memoize)
import Section.BlockingGetN as BlockingGetN
import Util.Util (styled)

boolful :: forall s m r lock payload
   . Korok s m
  => (AnEvent m Boolean /\ Domable m lock payload -> r) -> AnEvent m r
boolful i = keepLatest $ bus \push event' -> memoize (event' <|> pure true) \event ->
  i $ event /\ (D.div_ [D.button (click (event <#> (not >>> push)))
    [text (show <$> event)]])

intful :: forall s m r lock payload
   . Korok s m
  => (AnEvent m Int /\ Domable m lock payload -> r) -> AnEvent m r
intful i = keepLatest $ bus \push event' -> memoize (event' <|> pure 0) \event ->
  i $ event /\ (D.div_ [D.button (click (event <#> (add 1 >>> push)))
    [text (show <$> event)]])

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = DekuDo.do
  boolfulEv /\ boolfulEl <- envy <<< boolful
  intfulEv /\ intfulEl <- envy <<< intful
  D.div_ [
    boolfulEl, intfulEl,
    D.div_ [text (boolfulEv <#> show >>> ("Boolean result " <> _))],
    D.div_ [text (intfulEv <#> show >>> ("Int result " <> _))]
    ]

