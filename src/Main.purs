module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import FRP (dyn, subscribe)
import FRP.Ref as Ref

main :: Effect Unit
main = do
  rx <- Ref.new 1
  let x = Ref.value rx
  ry <- Ref.new 2
  let y = Ref.value ry
  rz <- Ref.new 3
  let z = Ref.value rz

  let xy = dyn $ x + y

  _ <- subscribe x (\v -> log $ "x = " <> show v)
  _ <- subscribe y (\v -> log $ "y = " <> show v)
  _ <- subscribe z (\v -> log $ "z = " <> show v)
  _ <- subscribe xy (\v -> log $ "x + y = " <> show v)
  _ <- subscribe (x + y + z) (\v -> log $ "x + y + z = " <> show v)

  Ref.write rx 10
  Ref.write ry 20
  Ref.write rz 30
