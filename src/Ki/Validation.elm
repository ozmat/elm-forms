module Ki.Validation exposing (..)

import Dict as D exposing (Dict)
import Ki.Field as F exposing (Field, Group)
import Ki.Value as V exposing (Value)


{- Generic implementation of validation -}
-- Error
-- TODO formError and maybe FormValidation for dictErrors ?


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
validation err valid a =
    if valid a then
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
andMap va vf =
    case ( va, vf ) of
        ( _, ValidationFailure err ) ->
            ValidationFailure err

        ( _, ValidationSuccess f ) ->
            map f va


andMapAcc : Validation err a -> Validation err (a -> b) -> Validation err b
andMapAcc va vf =
    case ( va, vf ) of
        ( ValidationSuccess _, ValidationFailure err ) ->
            ValidationFailure err

        ( ValidationFailure err1, ValidationFailure err2 ) ->
            ValidationFailure (append err1 err2)

        ( _, ValidationSuccess f ) ->
            map f va


succeed : a -> Validation err a
succeed a =
    ValidationSuccess a



{- Concrete usage of validation for Form -}
-- stringOnly : Value -> (String -> Validation err a) -> Validation (FormError err) String
-- boolOnly : Value -> (Bool -> Validation err a) -> Validation (FormError err) Bool
-- maybe : Maybe Value -> (Value -> Validation err a) -> Validation (FormError err) a
-- Validate


type alias Validate comparable err a =
    Group comparable -> Validation err a


required : Group comparable -> comparable -> (Maybe Value -> Validation err a) -> Validation err (a -> b) -> Validation err b
required fields comparable valid vf =
    andMapAcc (valid (F.getValue comparable fields)) vf


valid : a -> Validation err a
valid =
    succeed
