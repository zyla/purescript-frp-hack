module FRP where

import Prelude

import Data.Array as Array
import Data.Foldable (sequence_, for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Ref as ERef
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Any :: Type

class DynContext

type Dynamic a = DynContext => a

unsafeEval :: forall a. Dynamic a -> Effect a
unsafeEval = unsafeCoerce

dynEffect :: forall a. Effect a -> Dynamic a
dynEffect = unsafeCoerce

currentEvalDeps :: ERef.Ref (Maybe (Array SomeRef))
currentEvalDeps = unsafePerformEffect $ ERef.new Nothing

newtype Ref a = Ref { value :: ERef.Ref a, listeners :: ERef.Ref (Array (Effect Unit)) }

instance Eq (Ref a) where
  eq = unsafeRefEq

type SomeRef = Ref Any

toSomeRef :: forall a. Ref a -> SomeRef
toSomeRef = unsafeCoerce

refNew :: forall a. a -> Effect (Ref a)
refNew initialValue = do
  value <- ERef.new initialValue
  listeners <- ERef.new []
  pure $ Ref { value, listeners }

refValue :: forall a. Ref a -> Dynamic a
refValue ref@(Ref r) = dynEffect do
  m_deps <- ERef.read currentEvalDeps
  for_ m_deps \deps ->
    ERef.write (Just (deps <> [toSomeRef ref])) currentEvalDeps
  ERef.read r.value

refWrite :: forall a. Ref a -> a -> Effect Unit
refWrite (Ref r) v = do
  ERef.write v r.value
  listeners <- ERef.read r.listeners
  sequence_ listeners

readDynamic :: forall a. Dynamic a -> Effect a
readDynamic = unsafeEval

dyn :: forall a. Dynamic a -> Dynamic a
dyn = identity

subscribe :: forall a. Dynamic a -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribe d handler = do
  depsRef <- ERef.new []
  let unsubscribe = do
        oldDeps <- ERef.read depsRef
        for_ oldDeps \(Ref r) ->
          ERef.modify_ (Array.filter (\x -> not (x `unsafeRefEq` eval))) r.listeners
      eval = do
        ERef.write (Just []) currentEvalDeps
        value <- unsafeEval d
        newDeps <- fromMaybe [] <$> ERef.read currentEvalDeps
        ERef.write Nothing currentEvalDeps
        unsubscribe
        for_ newDeps \(Ref r) ->
          ERef.modify_ (flip Array.snoc eval) r.listeners
        ERef.write newDeps depsRef
        handler value
  eval
  pure unsubscribe
