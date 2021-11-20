module Broken where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import FRP (Dynamic, readDynamic)
import FRP.Ref as Ref

getter :: forall a. Dynamic (Dynamic a -> a)
getter x = x

main :: Effect Unit
main = do
  pureReadDynamic <- readDynamic getter

  xr <- Ref.new 1
  let x = Ref.value xr

  log $ show $ pureReadDynamic x

  Ref.write xr 2
  log $ show $ pureReadDynamic x
  -- Referential transparency broken!
  -- `pureReadDynamic` returns different values at different times.
