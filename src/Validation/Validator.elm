module Validation.Validator exposing (..)

import Array exposing (Array)
import Validation.Extensions.List as List


type alias Validator error value = value -> List error


-- * Utils
-------------------------

mapError : (a -> b) -> Validator a value -> Validator b value
mapError mapping validator value = validator value |> List.map mapping

mapValue : (b -> a) -> Validator error a -> Validator error b
mapValue mapping validator value = validator (mapping value)

mapFilterValue : (b -> Maybe a) -> Validator error a -> Validator error b
mapFilterValue mapping aValidator = mapping >> List.maybe >> List.concatMap aValidator

condition : (value -> Bool) -> error -> Validator error value
condition predicate error value = if predicate value then [] else [error]

-- ** Concatenation
-------------------------

{-|
Concatenate validators, producing a validator,
which only emits the first error,
and does not execute the remaining validators,
when an error is found.
Thus it is more efficient than `every`,
so prefer it when you're only interested
in one error message per validation.
-}
any : List (Validator error value) -> Validator error value
any validators value =
  let
    loop remainingValidators = case remainingValidators of
      validator :: nextValidators -> case validator value of
        errorsHead :: _ -> [errorsHead]
        _ -> loop nextValidators
      _ -> []
    in loop validators

{-|
Concatenate validators, gathering the results from all of them.
-}
every : List (Validator error value) -> Validator error value
every validators value =
  validators |>
  List.concatMap (\ validator -> validator value)


-- * Value projections
-------------------------

onArrayLength : Validator error Int -> Validator error (Array a)
onArrayLength = mapValue Array.length

onStringLength : Validator error Int -> Validator error String
onStringLength = mapValue String.length

onMaybeJust : Validator error a -> Validator error (Maybe a)
onMaybeJust = mapFilterValue identity


-- * Generic validators
-------------------------

isAtLeast : comparable -> Validator comparable comparable
isAtLeast threshold = condition ((<=) threshold) threshold

isAtMost : comparable -> Validator comparable comparable
isAtMost threshold = condition ((>=) threshold) threshold

isOneOf : List a -> Validator (List a) a
isOneOf options = condition (\ x -> List.member x options) options


-- * Specific validators
-------------------------

arrayLengthIsAtLeast : Int -> Validator Int (Array a)
arrayLengthIsAtLeast = isAtLeast >> onArrayLength

arrayLengthIsAtMost : Int -> Validator Int (Array a)
arrayLengthIsAtMost = isAtMost >> onArrayLength

stringLengthIsAtLeast : Int -> Validator Int String
stringLengthIsAtLeast = isAtLeast >> onStringLength

stringLengthIsAtMost : Int -> Validator Int String
stringLengthIsAtMost = isAtMost >> onStringLength
