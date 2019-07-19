module Validation.Extensions.Result exposing (..)


maybe : error -> Maybe ok -> Result error ok
maybe error maybeOk = case maybeOk of
  Just ok -> Ok ok
  Nothing -> Err error
