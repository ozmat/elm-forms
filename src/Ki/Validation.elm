module Ki.Validation exposing (..)

import Dict as D exposing (Dict)
import Ki.Field as F exposing (Field, Group)
import Ki.Value as V exposing (Value)


-- Error


type Error err
    = Error err
    | ErrorList (List err)


append : Error err -> Error err -> Error err
append e1 e2 =
    case ( e1, e2 ) of
        ( Error err1, Error err2 ) ->
            ErrorList [ err1, err2 ]

        ( Error err, ErrorList l ) ->
            ErrorList (err :: l)

        ( ErrorList l, Error err ) ->
            ErrorList (err :: l)

        ( ErrorList l1, ErrorList l2 ) ->
            ErrorList (l1 ++ l2)



-- Validation


type Validation err a
    = ValidationFailure (Error err)
    | ValidationSuccess a


validation : err -> (a -> Bool) -> a -> Validation err a
validation err test a =
    if test a then
        ValidationSuccess a
    else
        ValidationFailure (Error err)


map : (a -> b) -> Validation err a -> Validation err b
map f validation =
    case validation of
        ValidationSuccess a ->
            ValidationSuccess (f a)

        ValidationFailure err ->
            ValidationFailure err


andMap : Validation err a -> Validation err (a -> b) -> Validation err b
andMap va vb =
    case ( va, vb ) of
        ( _, ValidationFailure err ) ->
            ValidationFailure err

        ( _, ValidationSuccess f ) ->
            map f va


andMapAcc : Validation err a -> Validation err (a -> b) -> Validation err b
andMapAcc va vb =
    case ( va, vb ) of
        ( ValidationSuccess _, ValidationFailure err ) ->
            ValidationFailure err

        ( ValidationFailure err1, ValidationFailure err2 ) ->
            ValidationFailure (append err1 err2)

        ( _, ValidationSuccess f ) ->
            map f va
