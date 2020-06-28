module Helpers exposing (..)

ifElse : a -> a -> Bool -> a
ifElse x y bool =
  if bool then x else y
