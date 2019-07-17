module Validation.Refiner exposing (..)

import Array exposing (Array)
import Validation.Validator as Validator exposing (Validator)


{-|
Projection from `input` to `output`,
where `input` values can be of a broader range and thus fail to produce `output`.

Implemented as a mapping function which can fail.

This abstraction can be composed in all sorts of ways: monadically, alternatively, categorically.
-}
type alias Refiner error input output = input -> Result error output

succeed : output -> Refiner error input output
succeed output _ = Ok output

fail : error -> Refiner error input output
fail error _ = Err error

{-|
Lift a validator into a refiner,
which only restricts the domain of an existing value,
without mapping it into another type.
-}
validator : Validator error a -> Refiner error a a
validator x a = case x a of
  error :: _ -> Err error
  _ -> Ok a

{-|
Lift a mapping function,
which produces `output` from `input` without errors.
-}
mapping : (input -> output) -> Refiner error input output
mapping fn input = Ok (fn input)

identity : Refiner error a a
identity = Ok

compose : Refiner error b c -> Refiner error a b -> Refiner error a c
compose bc ab = ab >> Result.andThen bc

mapInput : (b -> a) -> Refiner error a output -> Refiner error b output
mapInput fn refinerA = fn >> refinerA

mapOutput : (a -> b) -> Refiner error input a -> Refiner error input b
mapOutput fn refinerA = refinerA >> Result.map fn

mapError : (a -> b) -> Refiner a input output -> Refiner b input output
mapError fn refinerA = refinerA >> Result.mapError fn

{-|
Sequentially execute two refiners on the same input.
-}
andThen : (a -> Refiner error input b) -> Refiner error input a -> Refiner error input b
andThen fn refinerA input = refinerA input |> Result.andThen (\ a -> fn a input)

{-|
Try one refiner and, if it fails, the other.
The one to the right goes first.
-}
or : Refiner error input output -> Refiner error input output -> Refiner error input output
or that this input = case this input of
  Ok output -> Ok output
  _ -> that input

{-|
Try a refiner or iterate thru a list of alternatives until the first one that succeeds,
using the error of the last one of them if all fail.
-}
orAny : List (Refiner error input output) -> Refiner error input output -> Refiner error input output
orAny tail head input = case head input of
  Ok output -> Ok output
  Err error -> any error tail input

{-|
Iterate thru refiners until the first one that succeeds,
using the error of the last one of them if all fail or
the specified one when the list is empty.
-}
any : error -> List (Refiner error input output) -> Refiner error input output
any error refiners input =
  let
    loop currentError currentRefiners = case currentRefiners of
      refiner :: nextRefiners -> case refiner input of
        Ok output -> Ok output
        Err nextError -> loop nextError nextRefiners
      _ -> Err currentError
    in loop error refiners

