module Validation.Extensions.List exposing (..)

import List exposing (..)


maybe : Maybe a -> List a
maybe x = case x of
  Just y -> [y]
  _ -> []

mapHeadAndTail : (a -> b) -> (a -> b) -> List a -> List b
mapHeadAndTail headFn tailFn list = case list of
  head :: tail -> headFn head :: map tailFn tail
  _ -> []

mapTail : (a -> a) -> List a -> List a
mapTail mapping list = case list of
  head :: tail -> head :: map mapping tail
  _ -> []

mapInit : (a -> a) -> List a -> List a
mapInit mapping =
  let
    build list = case list of
      head :: tail -> buildTail head [] tail
      _ -> []
    buildTail previous newList list = case list of
      head :: tail -> buildTail head (mapping previous :: newList) tail
      _ -> previous :: newList
    in build >> reverse

