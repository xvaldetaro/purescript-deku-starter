module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Ref as STRef
import Control.Plus (empty)
import Data.Array (cons)
import Data.Array as Array
import Data.Compactable (compact)
import Data.Foldable (oneOf, oneOfMap)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant, inj, on)
import Data.Variant.Internal (VariantRep(..))
import Deku.Attribute (Attribute)
import Deku.Control (dyn, text, text_)
import Deku.Core (Domable, Nut, insert_, remove)
import Deku.DOM as D
import Deku.Toplevel (runInBody, runInBody1)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (class Filterable, AnEvent, EventIO, create, filterMap, fold, fromEvent, keepLatest, makeEvent, mapAccum, subscribe)
import Foreign (Foreign)
import Foreign.Object (Object)
import Hyrule.Zora (Zora)
import Prim.Row as R
import Prim.RowList as RL
import Record (insert)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

data Some :: forall k. k -> Type
data Some r

asSome :: forall a b c. R.Union a b c => { | a } -> Some c
asSome = unsafeCoerce

toVariants :: forall r. Some r -> Array (Variant r)
toVariants some = foldrWithIndex (\key value -> cons (toVariant (VariantRep { type: key, value }))) [] (toObjectForeign some)
  where
  toVariant :: VariantRep Foreign -> Variant r
  toVariant = unsafeCoerce

  toObjectForeign :: Some r -> Object Foreign
  toObjectForeign = unsafeCoerce

toVariantsE :: forall r s m. MonadST s m => AnEvent m (Some r) -> AnEvent m (Variant r)
toVariantsE = keepLatest <<< map (oneOfMap pure <<< toVariants)

class Eventable (m :: Type -> Type) (rl :: RL.RowList Type) (i :: Row Type) (o :: Row Type) | rl -> i o where
  eventable :: Filterable (AnEvent m) => Proxy rl -> AnEvent m (Variant i) -> { | o }

instance Eventable m RL.Nil () () where
  eventable _ _ = {}

instance
  ( IsSymbol key
  , R.Lacks key i'
  , R.Lacks key o'
  , R.Cons key value i' i
  , R.Cons key (AnEvent m value) o' o
  , Eventable m rest i' o'
  ) =>
  Eventable m (RL.Cons key value rest) i o where
  eventable _ i = insert (Proxy :: _ key) (filterMap (on (Proxy :: _ key) Just (const Nothing)) i) (eventable (Proxy :: _ rest) (map shrinkVariant i))
    where
    shrinkVariant :: Variant i -> Variant i'
    shrinkVariant = unsafeCoerce

variantEvent :: forall m i o rl. Filterable (AnEvent m) => RL.RowToList i rl => Eventable m rl i o => AnEvent m (Variant i) -> { | o }
variantEvent = eventable (Proxy :: _ rl)

-- start joyride copy
fireAndForget
  :: forall s m
   . MonadST s m
  => AnEvent m ~> AnEvent m
fireAndForget = oneOff Just

oneOff
  :: forall s m a b
   . MonadST s m
  => (a -> Maybe b)
  -> AnEvent m a
  -> AnEvent m b
oneOff f e = compact $ emitUntil identity
  ( mapAccum
      ( \a b -> case f a, b of
          _, true -> true /\ Nothing
          Nothing, false -> false /\ Just Nothing
          Just x, false -> true /\ Just (Just x)
      )
      e
      false
  )

emitUntil
  :: forall s m a b
   . MonadST s m
  => (a -> Maybe b)
  -> AnEvent m a
  -> AnEvent m b
emitUntil aToB e = makeEvent \k -> do
  o <- subscribe (withUnsubscribe e) \{ unsubscribe, value } ->
    case aToB value of
      Just b -> k b
      Nothing -> unsubscribe
  pure o

withUnsubscribe :: forall s m a. MonadST s m => AnEvent m a -> AnEvent m { unsubscribe :: m Unit, value :: a }
withUnsubscribe e = makeEvent \ff -> do
  let f unsubscribe value = ff { unsubscribe, value }
  active <- liftST $ Ref.new true
  ro <- liftST $ Ref.new (pure unit)
  let
    cancel = do
      _ <- liftST $ Ref.write false active
      join (liftST $ Ref.read ro)
    f' = f cancel
    callback a = do
      whenM (liftST $ Ref.read active) (f' a)
  o <- subscribe e callback
  (liftST $ Ref.read active) >>= case _ of
    false -> o $> pure unit
    true -> liftST $ Ref.write o ro $> o

memoBeh :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBeh e a f = makeEvent \k -> do
  { push, event } <- create
  current <- liftST (STRef.new a)
  let
    writeVal v = liftST (STRef.write v current) :: m a
    event' = makeEvent \k' -> do
      liftST (STRef.read current) >>= k'
      subscribe event k'
  k (f event')
  subscribe e (\v -> writeVal v *> push v)

-- end joyride copy

loadingErrorDone
  :: forall element loading error done lock payload
   . (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Attribute element)
  -> AnEvent Zora (Variant (loading :: loading, error :: error, done :: done))
  -> { loading :: AnEvent Zora loading -> Domable lock payload, error :: AnEvent Zora error -> Domable lock payload, done :: AnEvent Zora done -> Domable lock payload }
  -> Domable lock payload
loadingErrorDone element atts variant matcher = dyn element atts $ oneOf
  [ fireAndForget ve.loading <#> \v -> oneOf [ pure (insert_ (matcher.loading (pure v <|> ve.loading))), ve.done $> remove, ve.error $> remove ]
  , fireAndForget ve.error <#> \v -> oneOf [ pure (insert_ (matcher.error (pure v <|> ve.error))) ]
  , fireAndForget ve.done <#> \v -> oneOf [ pure (insert_ (matcher.done (pure v <|> ve.done))) ]
  ]
  where
  ve = variantEvent variant

loadingErrorDone_
  :: forall element loading error done lock payload
   . (AnEvent Zora (Attribute element) -> Array (Domable lock payload) -> Domable lock payload)
  -> AnEvent Zora (Variant (loading :: loading, error :: error, done :: done))
  -> { loading :: AnEvent Zora loading -> Domable lock payload, error :: AnEvent Zora error -> Domable lock payload, done :: AnEvent Zora done -> Domable lock payload }
  -> Domable lock payload
loadingErrorDone_ element variant matcher = loadingErrorDone element empty variant matcher

main :: Effect Unit
main = do
  { event: event', push } :: EventIO (Variant (loading :: Unit, error :: Unit, done :: String)) <- create
  runInBody1
    (
      memoBeh (fromEvent event') (inj (Proxy :: _ "loading") unit)
        ( \event ->
            loadingErrorDone_ D.div event
              { loading: \_ -> text_ "Loading."
              , error: \_ -> text_ "Error."
              , done: \doc' -> do
                  let doc = fold (flip Array.snoc) doc' []
                  D.div_ [ text $ show <$> doc ]
              }
        )
    )
  launchAff_ do
    liftEffect $ push ( inj (Proxy :: _ "done") "First")
    liftEffect $ push ( inj (Proxy :: _ "done") "Second")
    liftEffect $ push ( inj (Proxy :: _ "done") "Third")