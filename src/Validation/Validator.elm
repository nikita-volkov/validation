module Validation.Validator exposing (..)

import Array exposing (Array)
import Either exposing (Either(..))
import Validation.Extensions.List as List
import Email


{-|
Abstraction over a function, which looks for errors in the value.
-}
type alias Validator value error = value -> List error


-- * Utils
-------------------------

mapError : (a -> b) -> Validator value a -> Validator value b
mapError mapping validator value = validator value |> List.map mapping

{-|
Produce a single error when the validator fails.
-}
setError : b -> Validator value a -> Validator value b
setError newError validator = validator >> \ errors -> if List.isEmpty errors then [] else [newError]

mapValue : (b -> a) -> Validator a error -> Validator b error
mapValue mapping validator value = validator (mapping value)

mapFilterValue : (b -> Maybe a) -> Validator a error -> Validator b error
mapFilterValue mapping = mapConcatValue (mapping >> List.maybe)

mapConcatValue : (b -> List a) -> Validator a error -> Validator b error
mapConcatValue mapping aValidator = mapping >> List.concatMap aValidator

-- ** Concatenation
-------------------------

none : Validator value error
none = always []

{-|
Combine validators in such a way that it's enough for one of them to be satisfied.
-}
any : List (Validator value error) -> Validator value error
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
firstOfEvery : List (Validator value error) -> Validator value error
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
allOfEvery : List (Validator value error) -> Validator value error
allOfEvery validators value =
  validators |>
  List.concatMap (\ validator -> validator value)


-- * Value projections
-------------------------

onArrayLength : Validator Int error -> Validator (Array a) error
onArrayLength = mapValue Array.length

onStringLength : Validator Int error -> Validator String error
onStringLength = mapValue String.length

onMaybeJust : Validator a error -> Validator (Maybe a) error
onMaybeJust = mapFilterValue identity

onArrayElement : Validator a error -> Validator (Array a) error
onArrayElement = onListElement >> mapValue Array.toList

onListElement : Validator a error -> Validator (List a) error
onListElement = List.concatMap

onStringChar : Validator Char error -> Validator String error
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

isInRange : comparable -> comparable -> Validator comparable (Either comparable comparable)
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

arrayLengthIsAtLeast : Int -> Validator (Array a) Int
arrayLengthIsAtLeast = isAtLeast >> onArrayLength

arrayLengthIsAtMost : Int -> Validator (Array a) Int
arrayLengthIsAtMost = isAtMost >> onArrayLength

stringLengthIsAtLeast : Int -> Validator String Int
stringLengthIsAtLeast = isAtLeast >> onStringLength

stringLengthIsAtMost : Int -> Validator String Int
stringLengthIsAtMost = isAtMost >> onStringLength

stringSatisfiesEmailSyntax : Validator String String
stringSatisfiesEmailSyntax = satisfies (Email.fromString >> (/=) Nothing)
