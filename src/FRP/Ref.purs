module FRP.Ref where

import Prelude

import Data.Foldable (sequence_, for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Ref as ERef
import FRP (Ref(..), currentEvalDeps, toSomeRef, Dynamic, dynEffect)

new :: forall a. a -> Effect (Ref a)
new initialValue = do
  value_ <- ERef.new initialValue
  listeners <- ERef.new []
  pure $ Ref { value: value_, listeners }

read :: forall a. Ref a -> Effect a
read (Ref r) = ERef.read r.value

value :: forall a. Ref a -> Dynamic a
value ref@(Ref r) = dynEffect do
  m_deps <- ERef.read currentEvalDeps
  for_ m_deps \deps ->
    ERef.write (Just (deps <> [toSomeRef ref])) currentEvalDeps
  ERef.read r.value

write :: forall a. Ref a -> a -> Effect Unit
write (Ref r) v = do
  ERef.write v r.value
  listeners <- ERef.read r.listeners
  sequence_ listeners
