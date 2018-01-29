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
-- TODO change FormValidation for dictErrors ? Or change FormError err to FormError comparable err


type FormError err
    = MissingField
    | WrongType
    | CustomError err


customFailure : err -> FormValidation err a
customFailure err =
    failure (CustomError err)


type alias FormValidation err a =
    Validation (FormError err) a


formValidation : err -> (a -> Bool) -> a -> FormValidation err a
formValidation err valid a =
    validation (CustomError err) valid a


missingField : (Value -> FormValidation err a) -> Maybe Value -> FormValidation err a
missingField valid mvalue =
    case mvalue of
        Nothing ->
            failure MissingField

        Just value ->
            valid value



-- TODO Use a Type FieldType ?
-- TODO Add int parsing and float parsing
-- TODO Add basic validation (string ones)
-- TODO refactor "valid"


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



{- Validate a Form -}


type alias Validate comparable err a =
    Group comparable -> FormValidation err a


valid : a -> FormValidation err a
valid =
    success



{- Validate a Field -}


fieldValid : Group comparable -> comparable -> (Value -> FormValidation err a) -> FormValidation err a
fieldValid fields comparable valid =
    missingField valid (F.getValue comparable fields)



-- Required


required : Group comparable -> comparable -> (Value -> FormValidation err a) -> FormValidation err (a -> b) -> FormValidation err b
required fields comparable valid fvf =
    andMap (fieldValid fields comparable valid) fvf


requiredAcc : Group comparable -> comparable -> (Value -> FormValidation err a) -> FormValidation err (a -> b) -> FormValidation err b
requiredAcc fields comparable valid fvf =
    andMapAcc (fieldValid fields comparable valid) fvf



-- Hardcoded, equivalent to :
-- ```|> required fields comparable (\_ -> valid a)```


harcoded : Group comparable -> comparable -> a -> FormValidation err (a -> b) -> FormValidation err b
harcoded fields comparable a fvf =
    andMap (fieldValid fields comparable (\_ -> valid a)) fvf


harcodedAcc : Group comparable -> comparable -> a -> FormValidation err (a -> b) -> FormValidation err b
harcodedAcc fields comparable a fvf =
    andMapAcc (fieldValid fields comparable (\_ -> valid a)) fvf



-- Optional, equivalent to :
-- a "string only required" that returns default if empty or validates the string otherwise
-- TODO implement optional for more than just String


optional_ : (String -> FormValidation err a) -> a -> (Value -> FormValidation err a)
optional_ svalid default =
    \value ->
        stringField
            (\s ->
                if String.isEmpty s then
                    valid default
                else
                    svalid s
            )
            value


optional : Group comparable -> comparable -> (String -> FormValidation err a) -> a -> FormValidation err (a -> b) -> FormValidation err b
optional fields comparable valid default fvf =
    andMap (fieldValid fields comparable (optional_ valid default)) fvf


optionalAcc : Group comparable -> comparable -> (String -> FormValidation err a) -> a -> FormValidation err (a -> b) -> FormValidation err b
optionalAcc fields comparable valid default fvf =
    andMapAcc (fieldValid fields comparable (optional_ valid default)) fvf



-- OptionalMaybe, equivalent to :
-- ```|> optional fields comparable (\s -> ... Just s) Nothing```


optionalMaybe : Group comparable -> comparable -> (String -> FormValidation err a) -> FormValidation err (Maybe a -> b) -> FormValidation err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> map Just (valid s)) Nothing fvf


optionalMaybeAcc : Group comparable -> comparable -> (String -> FormValidation err a) -> FormValidation err (Maybe a -> b) -> FormValidation err b
optionalMaybeAcc fields comparable valid fvf =
    optionalAcc fields comparable (\s -> map Just (valid s)) Nothing fvf
