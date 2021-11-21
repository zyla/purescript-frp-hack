# Ergonomic FRP for PureScript?

Typically in Haskell-like languages the API for FRP-like libraries centers around a type, called `Behavior` or `Dynamic`, which represents a dynamically-changing value, and is a `Functor`/`Applicative`/`Monad`. Composing derived values involves the usual monadic rituals (`do`/`ado` notation, `<$>`/`<*>`, `traverse` instead of `map` etc.).
However, dynamically changing values are conceptually very similar to pure computations - the `Dynamic` monad is commutative and idempotent. So it's sad that we can't write `Dynamic` computations just like the pure ones.

Or maybe we can? In JavaScript land (e.g. KnockoutJS, MobX) it is common to have observables which are themselves functions. Calling an observable reads its value, and if a dependent observable evaluation is in progress, registers a dependency. This API style has the advantage of having minimal noise (a function call with no arguments to read a value).

We can encode this pattern in PureScript. But since reading a `Dynamic` is not a pure operation, we should disallow it in pure functions at the type level. This can be done by introducting a special type class:

```purescript
class DynContext

type Dynamic a = DynContext => a
```

Operationally `Dynamic a` is a function `DynContext -> a`, just like a Knockout Observable. These values can be composed without extra ceremony:

```purescript
foo :: Dynamic Int -> Dynamic Int -> Dynamic Int
foo x y = x + y + 1
```

## Does this really work?

Sort of, but I wouldn't use it due to type safety holes (see below). See the [Main](./src/Main.purs) module for an example.

Note that this is not a good FRP implementation; it is not glitch-free, doesn't support batching, probably has a ton of reentrancy bugs. It's just meant to showcase this API style.

## Is this type safe?

**No**, this hack can easily be broken by leaking the `DynContext` through a lambda:

```purescript
getter :: forall a. Dynamic (Dynamic a -> a)
getter x = x
```

See the [Broken](./src/Broken.purs) module for a complete example that demonstrates this.

Reading the above Dynamic gives us a function which can read a Dynamic in a pure context, breaking referential transparency.

This is not surprising, since we're abusing the typeclass mechanism as something like an effect system.

### Can this be fixed?

I believe if we modified the type system to disallow propagation of "effect-like" constraints through lambda abstractions, that would solve the problem.
However, this would limit expressiveness: there are certain places where we want to use a lambda, for example:

```purescript
foo :: Dynamic (Array Int) -> Dynamic Int -> Dynamic (Array Int)
foo xs y = map (\x -> x + y) xs
```

In this case it's safe to do because the lambda doesn't escape the DynContext scope.
But tracking this would require somehow annotating this at the type level, and that gets complicated.
