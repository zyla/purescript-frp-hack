module FRP where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Ref as ERef
import Effect.Unsafe (unsafePerformEffect)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- | A constraint used to delimit `Dynamic` computations.
-- | If `DynContext` is in scope, it means we're defining a `Dynamic`.
class DynContext

-- | `Dynamic a` represents a computation resulting in a value of type `a`, which can be read or subscribed to.
-- |
-- | Since this is just `a` with an additional constrants, `Dynamics` can be composed like ordinary expressions:
-- |
-- | ```
-- | x, y :: Dynamic Int
-- | x + y :: Dynamic Int
-- | ```
-- | 
-- | However, when defining let-bindings, PureScript fails to generalize over the DynContext constraint,
-- | so this fails:
-- |
-- | ```
-- | let z = x + y
-- | ```
-- |
-- | However, adding an explicit type signature solved the problem.
-- | Alternatively the definition can be wrapped in the `dyn` helper, which accomplishes the same thing.
-- |
-- | ```
-- | let z = dyn $ x + y
-- | ```
type Dynamic a = DynContext => a

-- | Read the current value of a `Dynamic`.
readDynamic :: forall a. Dynamic a -> Effect a
readDynamic = unsafeEval

-- | A helper for defining Dynamics.
dyn :: forall a. Dynamic a -> Dynamic a
dyn = identity

-- | A mutable cell which can be read as a Dynamic.
newtype Ref a = Ref { value :: ERef.Ref a, listeners :: ERef.Ref (Array (Effect Unit)) }

instance Eq (Ref a) where
  eq = unsafeRefEq

-- | Subscribe to changes in the value of a `Dynamic`.
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

-- internals

foreign import data Any :: Type

type SomeRef = Ref Any

toSomeRef :: forall a. Ref a -> SomeRef
toSomeRef = unsafeCoerce

unsafeEval :: forall a. Dynamic a -> Effect a
unsafeEval = unsafeCoerce

dynEffect :: forall a. Effect a -> Dynamic a
dynEffect = unsafeCoerce

currentEvalDeps :: ERef.Ref (Maybe (Array SomeRef))
currentEvalDeps = unsafePerformEffect $ ERef.new Nothing
