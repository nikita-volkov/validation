module Validation.Validator exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import Validation.Extensions.List as List


{-|
Abstraction over a function, which looks for errors in the value.
-}
type alias Validator error value = value -> List error


-- * Utils
-------------------------

mapError : (a -> b) -> Validator a value -> Validator b value
mapError mapping validator value = validator value |> List.map mapping

{-|
Produce a single error when the validator fails.
-}
setError : b -> Validator a value -> Validator b value
setError newError validator = validator >> \ errors -> if List.isEmpty errors then [] else [newError]

mapValue : (b -> a) -> Validator error a -> Validator error b
mapValue mapping validator value = validator (mapping value)

mapFilterValue : (b -> Maybe a) -> Validator error a -> Validator error b
mapFilterValue mapping = mapConcatValue (mapping >> List.maybe)

mapConcatValue : (b -> List a) -> Validator error a -> Validator error b
mapConcatValue mapping aValidator = mapping >> List.concatMap aValidator

-- ** Concatenation
-------------------------

none : Validator error value
none = always []

{-|
Combine validators in such a way that it's enough for one of them to be satisfied.
-}
any : List (Validator error value) -> Validator error value
any validators value =
  let
    loop errors currentValidators = case currentValidators of
      validator :: nextValidators -> case validator value of
        [] -> []
        newErrors -> loop (newErrors ++ errors) nextValidators
      _ -> errors
    in loop [] validators

{-|
Concatenate validators, producing a validator,
which only emits the first error,
and does not execute the remaining validators,
when an error is found.
Thus it is more efficient than `allOfEvery`,
so prefer it when you're only interested
in one error message per validation.
-}
firstOfEvery : List (Validator error value) -> Validator error value
firstOfEvery validators value =
  let
    loop currentValidators = case currentValidators of
      validator :: nextValidators -> case validator value of
        errorsHead :: _ -> [errorsHead]
        _ -> loop nextValidators
      _ -> []
    in loop validators

{-|
Concatenate validators, gathering the results from all of them.
-}
allOfEvery : List (Validator error value) -> Validator error value
allOfEvery validators value =
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

satisfies : (value -> Bool) -> Validator value value
satisfies predicate value = if predicate value then [] else [value]

equals : a -> Validator a a
equals expected = satisfies ((==) expected)

isAtLeast : comparable -> Validator comparable comparable
isAtLeast threshold = satisfies ((<=) threshold)

isAtMost : comparable -> Validator comparable comparable
isAtMost threshold = satisfies ((>=) threshold)

isInRange : comparable -> comparable -> Validator (Either comparable comparable) comparable
isInRange min max =
  firstOfEvery
    [
      mapError Left (isAtLeast min),
      mapError Right (isAtMost max)
    ]

isOneOf : List a -> Validator a a
isOneOf options = satisfies (\ x -> List.member x options)


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
