module Ki.Validation exposing (..)

import Vi.Validation as VA exposing (Validation(..))
import Ki.Field as F exposing (Group)
import Ki.Value as V exposing (Value)


{- Concrete usage of validation for Form -}
-- Field validation
-- TODO refactor "valid"
-- TODO side-effect select validation ? Use the update messages to implement custom code ?


type FieldError err
    = MissingField
    | WrongType
    | CustomError err
    | NotEqual
    | NotInt
    | NotFloat


type alias FieldValidation err a =
    Validation (FieldError err) a


customFailure : err -> FieldValidation err a
customFailure err =
    VA.failure (CustomError err)


validF : a -> FieldValidation err a
validF =
    VA.success


fieldValidation : err -> (a -> Bool) -> a -> FieldValidation err a
fieldValidation err valid a =
    VA.validation (CustomError err) valid a


missingField : (Value -> FieldValidation err a) -> Maybe Value -> FieldValidation err a
missingField valid mvalue =
    case mvalue of
        Nothing ->
            VA.failure MissingField

        Just value ->
            valid value



-- TODO Use a Type FieldType ? To replace stringField, boolField and detail WrongType


stringField : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringField valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            VA.failure WrongType


boolField : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolField valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            VA.failure WrongType



-- TODO will not work with optional, fix it


intField : (Int -> FieldValidation err a) -> Value -> FieldValidation err a
intField valid value =
    stringField
        (\s ->
            case String.toInt s of
                Ok i ->
                    valid i

                Err _ ->
                    VA.failure NotInt
        )
        value


floatField : (Float -> FieldValidation err a) -> Value -> FieldValidation err a
floatField valid value =
    stringField
        (\s ->
            case String.toFloat s of
                Ok f ->
                    valid f

                Err _ ->
                    VA.failure NotFloat
        )
        value



-- TODO Add basic validation (email, length)


passwordFields : Value -> Value -> FieldValidation err String
passwordFields password passwordA =
    case ( password, passwordA ) of
        ( V.String s1, V.String s2 ) ->
            if s1 == s2 then
                VA.success s1
            else
                VA.failure NotEqual

        _ ->
            VA.failure WrongType



-- Form validation


type FormError comparable err
    = FormError comparable (FieldError err)


type alias FormValidation comparable err a =
    VA.Validation (FormError comparable err) a


toTuple : FormError comparable err -> ( comparable, FieldError err )
toTuple (FormError comparable fe) =
    ( comparable, fe )


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    case fv of
        Failure fe ->
            Failure (VA.mapError (FormError comparable) fe)

        Success a ->
            Success a



{- Validate a Form -}


type alias Validate comparable err a =
    Group comparable -> FormValidation comparable err a


valid : a -> FormValidation comparable err a
valid =
    VA.success



{- Validate a Field -}


fieldValid : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable valid =
    mapFormError comparable (missingField valid (F.getValue comparable fields))



-- Required


required : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable valid fvf =
    VA.andMap (fieldValid fields comparable valid) fvf


requiredAcc : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
requiredAcc fields comparable valid fvf =
    VA.andMapAcc (fieldValid fields comparable valid) fvf



-- Hardcoded, equivalent to :
-- ```|> required fields comparable (\_ -> valid a)```


harcoded : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcoded fields comparable a fvf =
    VA.andMap (fieldValid fields comparable (\_ -> validF a)) fvf


harcodedAcc : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcodedAcc fields comparable a fvf =
    VA.andMapAcc (fieldValid fields comparable (\_ -> validF a)) fvf



-- Optional, equivalent to :
-- a "string only required" that returns default if empty or validates the string otherwise


optional_ : (String -> FieldValidation err a) -> a -> (Value -> FieldValidation err a)
optional_ svalid default =
    \value ->
        stringField
            (\s ->
                if String.isEmpty s then
                    validF default
                else
                    svalid s
            )
            value


optional : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable valid default fvf =
    VA.andMap (fieldValid fields comparable (optional_ valid default)) fvf


optionalAcc : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optionalAcc fields comparable valid default fvf =
    VA.andMapAcc (fieldValid fields comparable (optional_ valid default)) fvf



-- OptionalMaybe, equivalent to :
-- ```|> optional fields comparable (\s -> ... Just s) Nothing```


optionalMaybe : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> VA.map Just (valid s)) Nothing fvf


optionalMaybeAcc : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybeAcc fields comparable valid fvf =
    optionalAcc fields comparable (\s -> VA.map Just (valid s)) Nothing fvf



{- Validate two fields -}
-- TODO implement a generic version (x fields) if this feature is used


fieldsValid : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
fieldsValid fields comparable1 comparable2 valid =
    let
        missing1 =
            FormError comparable1 MissingField

        missing2 =
            FormError comparable2 MissingField
    in
        case ( F.getValue comparable1 fields, F.getValue comparable2 fields ) of
            ( Nothing, Just _ ) ->
                VA.failure missing1

            ( Just _, Nothing ) ->
                VA.failure missing2

            ( Nothing, Nothing ) ->
                Failure (VA.ErrorList [ missing1, missing2 ])

            ( Just value1, Just value2 ) ->
                mapFormError comparable1 (valid value1 value2)


twoFields : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 valid fvf =
    VA.andMap (fieldsValid fields comparable1 comparable2 valid) fvf


twoFieldsAcc : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFieldsAcc fields comparable1 comparable2 valid fvf =
    VA.andMapAcc (fieldsValid fields comparable1 comparable2 valid) fvf



{- Validate a Group -}


missingGroup : (Group comparable -> FormValidation comparable err a) -> comparable -> Maybe (Group comparable) -> FormValidation comparable err a
missingGroup valid comparable mgroup =
    case mgroup of
        Nothing ->
            mapFormError comparable (VA.failure MissingField)

        Just value ->
            valid value


groupValid : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable valid =
    missingGroup valid comparable (F.getGroup comparable fields)


fieldGroup : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup fields comparable valid fvf =
    VA.andMap (groupValid fields comparable valid) fvf


fieldGroupAcc : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroupAcc fields comparable valid fvf =
    VA.andMapAcc (groupValid fields comparable valid) fvf
