module Main where

import Prelude

import Bolson.Core (envy)
import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Control.Plus (empty)
import Data.Array (drop, length, mapWithIndex)
import Data.Array as Array
import Data.Foldable (oneOfMap)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst)
import Data.Tuple.Nested ((/\))
import Deku.Attribute (Attribute)
import Deku.Control (dyn, text_)
import Deku.Core (Domable, Nut, insert, insert_, remove)
import Deku.DOM as D
import Deku.Do (useState')
import Deku.Do as Doku
import Deku.Listeners (textInput)
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Effect.Random (random)
import FRP.Event (AnEvent, ZoraEvent, filterMap, fold, fromEvent, keepLatest, mapAccum, memoize, withLast)
import Hyrule.Zora (Zora)
import Paraglider.Operator.FromEffect (fromEffect)
import Paraglider.Operator.MemoBeh (memoBeh')
import Paraglider.Operator.SwitchMap (switchMap)
import Style (bangCss, btn, css, inputCss, inputText)

-- | Renders an Array of items with its original order. This function is used to display an Array of
-- | Models that you have no control of. Say you get all the items in your list from the backend and
-- | you just need to simply render them as they come. The items don't have any control over when
-- | they are added or removed, it all comes from upstream.
-- |
-- | Accumulates diffs between `upstr`'s emissions and insert<<<render/reorder/remove items from
-- | each diff.
dynDiffOrdered :: ∀ element lock payload a id
  . Ord id
  => (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffOrdered elem attrs getId render upstr = dyn elem attrs dynEv
  where
  indexedUpstr :: ZoraEvent (Array (Tuple Int a))
  indexedUpstr = mapWithIndex Tuple <$> upstr

  getId' (Tuple _ item) = getId item
  dynEv = keepLatest $ memoize (diffAccum getId' indexedUpstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      -- | need to sort otherwise insertions go in wrong order

      sortedAddedEv :: ZoraEvent (Array (Tuple Int a))
      sortedAddedEv = (\{added} -> Array.sortWith fst $ Array.fromFoldable added) <$> diffEv

      itemsAddedUnfoldedEv :: ZoraEvent (Tuple Int a)
      itemsAddedUnfoldedEv = keepLatest $ (oneOfMap pure) <$> sortedAddedEv

      mkRow :: Tuple Int a -> ZoraEvent (Bolson.Child _ _ Zora _)
      mkRow (Tuple i item) = (pure $ insert i $ render item)
        <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

-- | Same as dynDiffOrdered, but always adds new elements to the end of the list
dynDiffUnordered :: ∀ element lock payload a id
  . Ord id
  => (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> id)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffUnordered elem attrs getId render upstr = dyn elem attrs dynEv
  where
  dynEv = keepLatest $ memoize (diffAccum getId upstr) \diffEv ->
    let
      mkOnSelfRemovedEv selfId = diffEv # filterMap \{removed} ->
        if Map.member selfId removed then Just remove else Nothing

      itemsAddedUnfoldedEv = switchMap (\{added} -> oneOfMap pure added) diffEv

      mkRow item = (pure $ insert_  $ render item) <|> mkOnSelfRemovedEv (getId item)
    in
    mkRow <$> itemsAddedUnfoldedEv

-- | Same as dynDiffOrdered, but assumes that `upstr`'s emission is an Array where elements are only
-- | added, never removed. Because of that it doesn't use any sorting or Map
dynDiffOnlyAddition :: ∀ element lock payload a
  . (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> (a -> Domable lock payload)
  -> AnEvent Zora (Array a)
  -> Domable lock payload
dynDiffOnlyAddition elem attrs render upstr = dyn elem attrs rowsEv
  where
  goNewItems {last, now} =
    let
      last' = fromMaybe [] last
    in
      if length last' < length now then Just (drop (length last') now) else Nothing

  newItemsDiffEv :: ZoraEvent (Array a)
  newItemsDiffEv = filterMap goNewItems (withLast $ upstr)

  newItemsUnfoldedEv :: ZoraEvent a
  newItemsUnfoldedEv = keepLatest $ (oneOfMap pure) <$> newItemsDiffEv

  mkRow :: a -> ZoraEvent (Bolson.Child _ _ Zora _)
  mkRow item = (pure $ insert_ $ render item)

  rowsEv :: ZoraEvent (ZoraEvent (Bolson.Child _ _ Zora _))
  rowsEv = mkRow <$> newItemsUnfoldedEv

diffAccum :: ∀ a id
  . Ord id
  => (a -> id)
  -> ZoraEvent (Array a)
  -> ZoraEvent { added :: Map id a, removed :: Map id a, all :: Map id a }
diffAccum getId upst = mapAccum go upst Map.empty
  where
  go incomingArr accDict =
    let
        incomingDict = Map.fromFoldable $ (\item -> Tuple (getId item) item) <$> incomingArr
        newItemsDict = Map.difference incomingDict accDict
        removedItemsDict = Map.difference accDict incomingDict
    in Tuple
      incomingDict
      { added: newItemsDict
      , removed: removedItemsDict
      , all: incomingDict
      }

nut :: Nut
nut = Doku.do
  p /\ e' <- useState'
  pText /\ textEv <- useState'
  let
    e = e' <|> pure {id: "", name: "", add: false}
    go :: String -> Effect (Tuple String ({id:: String, name::String}))
    go name = random <#> \r -> Tuple (show r) {id: (show r), name}

    go1 = traverse go ["a", "h", "m", "p", "w"]

  initial <- envy <<< (memoBeh' $ fromEvent $ fromEffect $ go1)


  let
    getId item = item.id
    foldGo {name, id, add} = if add then Map.insert id {name, id} else Map.delete id
    dictEv = initial # switchMap \i -> fold foldGo e (Map.fromFoldable i)
    arrEv =  Array.sortWith (_.name) <<< Array.fromFoldable <$> dictEv
    render {name, id} = D.div (bangCss "text-white")
      [ text_ $ "Name: " <> name <> " id:" <> id
      , btn "Remove" (css "bg-red-800") (pure $ p {id, name: "", add:false})
      ]

  D.div (bangCss "bg-gray-700 h-screen")
    [ inputText ((textInput $ pure pText) <|> bangCss inputCss)
    , btn "Add new" "" ((\t -> random >>= \r -> p {name: t, id: show r, add: true}) <$> textEv)
    , D.div (bangCss "text-xl text-white") [text_ "dynDiffOrdered"]
    , dynDiffOrdered D.div empty getId render arrEv
    , D.div (bangCss "text-xl text-white") [text_ "dynDiffUnordered"]
    , dynDiffUnordered D.div empty getId render arrEv
    , D.div (bangCss "text-xl text-white") [text_ "dynDiffAdditionOnly"]
    , dynDiffOnlyAddition D.div empty render arrEv
    ]

main :: Effect Unit
main = runInBody nut
