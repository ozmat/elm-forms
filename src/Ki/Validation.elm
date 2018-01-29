module Ki.Validation exposing (..)

import Ki.Field as F exposing (Field, Group)
import Ki.Value as V exposing (Value)


{- Generic implementation of validation -}
-- Error
-- TODO formError and maybe FormValidation for dictErrors ?


type ValidationError err
    = Error err
    | ErrorList (List err)


append : ValidationError err -> ValidationError err -> ValidationError err
append ve1 ve2 =
    case ( ve1, ve2 ) of
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
    = Failure (ValidationError err)
    | Success a


failure : err -> Validation err a
failure err =
    Failure (Error err)


success : a -> Validation err a
success a =
    Success a


validation : err -> (a -> Bool) -> a -> Validation err a
validation err valid a =
    if valid a then
        Success a
    else
        Failure (Error err)


map : (a -> b) -> Validation err a -> Validation err b
map f validation =
    case validation of
        Success a ->
            Success (f a)

        Failure ve ->
            Failure ve


andMap : Validation err a -> Validation err (a -> b) -> Validation err b
andMap va vf =
    case ( va, vf ) of
        ( _, Failure ve ) ->
            Failure ve

        ( _, Success f ) ->
            map f va


andMapAcc : Validation err a -> Validation err (a -> b) -> Validation err b
andMapAcc va vf =
    case ( va, vf ) of
        ( Success _, Failure ve ) ->
            Failure ve

        ( Failure ve1, Failure ve2 ) ->
            Failure (append ve1 ve2)

        ( _, Success f ) ->
            map f va



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
    success
