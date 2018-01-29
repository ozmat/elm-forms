module Ki.Validation exposing (..)

import Ki.Field as F exposing (Group)
import Ki.Value as V exposing (Value)


{- Generic implementation of validation -}
-- Error


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
-- TODO formError and maybe FormValidation for dictErrors ?


type alias FormValidation err a =
    Validation (FormError err) a


type FormError err
    = MissingField
    | WrongType
    | CustomError err


formFailure : FormError err -> FormValidation err a
formFailure fe =
    failure fe


customFailure : err -> FormValidation err a
customFailure err =
    failure (CustomError err)


maybeField : (Value -> FormValidation err a) -> Maybe Value -> FormValidation err a
maybeField valid mvalue =
    case mvalue of
        Nothing ->
            failure MissingField

        Just value ->
            valid value


stringField : (String -> FormValidation err a) -> Value -> FormValidation err a
stringField valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            failure WrongType


boolField : (Bool -> FormValidation err a) -> Value -> FormValidation err a
boolField valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            failure WrongType



-- Validate


type alias Validate comparable err a =
    Group comparable -> FormValidation err a


required : Group comparable -> comparable -> (Value -> FormValidation err a) -> FormValidation err (a -> b) -> FormValidation err b
required fields comparable valid vf =
    andMap (maybeField valid (F.getValue comparable fields)) vf


requiredAcc : Group comparable -> comparable -> (Value -> FormValidation err a) -> FormValidation err (a -> b) -> FormValidation err b
requiredAcc fields comparable valid vf =
    andMapAcc (maybeField valid (F.getValue comparable fields)) vf


valid : a -> FormValidation err a
valid =
    success
