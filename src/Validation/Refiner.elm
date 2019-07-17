module Validation.Refiner exposing (..)

import Array exposing (Array)
import Validation.Validator as Validator exposing (Validator)


{-|
Projection from `input` to `output`,
where `input` values can be of a broader range and thus fail to produce `output`.

Implemented as a mapping function which can fail and list the errors.
-}
type alias Refiner error input output = input -> Result (List error) output

succeed : output -> Refiner error input output
succeed output _ = Ok output

fail : error -> Refiner error input output
fail error _ = Err [error]

none : Refiner error input output
none _ = Err []

{-|
Lift a validator into a refiner,
which only restricts the domain of an existing value,
without mapping it into another type.
-}
validate : Validator error value -> Refiner error value value
validate validator value = case validator value of
  [] -> Ok value
  errors -> Err errors

{-|
Lift a mapping function,
which produces `output` from `input` without errors.
-}
map : (input -> output) -> Refiner error input output
map fn input = Ok (fn input)

{-|
Iterate thru refiners until the first one that succeeds,
collecting all errrors if all of them fail.
-}
any : List (Refiner error input output) -> Refiner error input output
any list input =
  let
    loop errList currentRefinerList = case currentRefinerList of
      currentRefiner :: remainingRefinerList -> case currentRefiner input of
        Ok output -> Ok output
        Err currentErrList -> loop (currentErrList ++ errList) remainingRefinerList
      _ -> Err errList
    in loop [] list

mapInput : (b -> a) -> Refiner error a output -> Refiner error b output
mapInput fn refinerA = fn >> refinerA

mapOutput : (a -> b) -> Refiner error input a -> Refiner error input b
mapOutput fn refinerA = refinerA >> Result.map fn
