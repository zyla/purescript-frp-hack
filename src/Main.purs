module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import FRP

main :: Effect Unit
main = do
  rx <- refNew 1
  let x = refValue rx
  ry <- refNew 2
  let y = refValue ry
  rz <- refNew 3
  let z = refValue rz

  let xy = dyn $ x + y

  _ <- subscribe x (\v -> log $ "x = " <> show v)
  _ <- subscribe y (\v -> log $ "y = " <> show v)
  _ <- subscribe z (\v -> log $ "z = " <> show v)
  _ <- subscribe xy (\v -> log $ "x + y = " <> show v)
  _ <- subscribe (x + y + z) (\v -> log $ "x + y + z = " <> show v)

  refWrite rx 10
  refWrite ry 20
  refWrite rz 30

  pure unit
