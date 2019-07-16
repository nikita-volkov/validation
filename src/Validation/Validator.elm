module Validation.Validator exposing (..)

import Array exposing (Array)
import Validation.Extensions.List as List


{-|
Abstraction over a function, which looks for errors in the value.
-}
type alias Validator error value = value -> List error


-- * Utils
-------------------------

mapError : (a -> b) -> Validator a value -> Validator b value
mapError mapping validator value = validator value |> List.map mapping

mapValue : (b -> a) -> Validator error a -> Validator error b
mapValue mapping validator value = validator (mapping value)

mapFilterValue : (b -> Maybe a) -> Validator error a -> Validator error b
mapFilterValue mapping = mapConcatValue (mapping >> List.maybe)

mapConcatValue : (b -> List a) -> Validator error a -> Validator error b
mapConcatValue mapping aValidator = mapping >> List.concatMap aValidator

condition : (value -> Bool) -> Validator value value
condition predicate value = if predicate value then [] else [value]

conditionAndError : (value -> Bool) -> error -> Validator error value
conditionAndError predicate error value = if predicate value then [] else [error]

-- ** Concatenation
-------------------------

none : Validator error value
none = always []

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

onArrayElement : Validator error a -> Validator error (Array a)
onArrayElement = onListElement >> mapValue Array.toList

onListElement : Validator error a -> Validator error (List a)
onListElement = List.concatMap

onStringChar : Validator error Char -> Validator error String
onStringChar = onListElement >> mapValue String.toList


-- * Generic validators
-------------------------

isAtLeast : comparable -> Validator comparable comparable
isAtLeast threshold = condition ((<=) threshold)

isAtMost : comparable -> Validator comparable comparable
isAtMost threshold = condition ((>=) threshold)

isInRange : comparable -> comparable -> Validator (comparable, comparable) comparable
isInRange min max =
  any
    [
      mapError (always (min, max)) (isAtLeast min),
      mapError (always (min, max)) (isAtMost max)
    ]

isOneOf : List a -> Validator a a
isOneOf options = condition (\ x -> List.member x options)


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
