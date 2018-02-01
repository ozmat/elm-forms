module Ki.Validation
    exposing
        ( -- Field validation
          FieldError(..)
        , failure
        , customFailure
        , success
        , validation
          -- Basic validation
        , stringValid
        , boolValid
        , int
        , float
        , email
        , length
        , passwordMatch
          -- Form validation
        , FormError
        , valid
        , toTuple
          -- Validate
        , Validate
        , required
        , required1
        , harcoded
        , harcoded1
        , optional
        , optional1
        , optionalMaybe
        , optionalMaybe1
        , twoFields
        , twoFields1
        , fieldGroup
        , fieldGroup1
        )

import Regex
import Vi.Validation as VA exposing (Validation(..))
import Ki.Field as F exposing (Group)
import Ki.Value as V exposing (Value)


-- TODO side-effect select validation ? Use the update messages to implement custom code ?
{- Field validation -}


type FieldError err
    = CustomError err
    | MissingField
    | WrongType
    | NotInt
    | NotFloat
    | NotEmail
    | NotLength
    | NotEqual


type alias FieldValidation err a =
    Validation (FieldError err) a



-- Helpers


failure : FieldError err -> FieldValidation err a
failure =
    VA.failure


customFailure : err -> FieldValidation err a
customFailure err =
    VA.failure (CustomError err)


success : a -> FieldValidation err a
success =
    VA.success


validation : err -> (a -> Bool) -> a -> FieldValidation err a
validation err valid a =
    VA.validation (CustomError err) valid a



-- Type validation helpers


stringValid : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringValid valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            VA.failure WrongType


boolValid : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolValid valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            VA.failure WrongType



-- Type-casting validation helpers


int : (Int -> FieldValidation err a) -> String -> FieldValidation err a
int valid s =
    case String.toInt s of
        Ok i ->
            valid i

        Err _ ->
            VA.failure NotInt


float : (Float -> FieldValidation err a) -> String -> FieldValidation err a
float valid s =
    case String.toFloat s of
        Ok f ->
            valid f

        Err _ ->
            VA.failure NotFloat



-- Basic validation helpers


email : (String -> FieldValidation err a) -> String -> FieldValidation err a
email valid s =
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input/email
    if Regex.contains (Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$") s then
        valid s
    else
        VA.failure NotEmail


length : Int -> Int -> (String -> FieldValidation err a) -> String -> FieldValidation err a
length low high valid s =
    let
        len =
            String.length s
    in
        if len > low && len < high then
            valid s
        else
            -- TODO returns the length ?
            VA.failure NotLength


passwordMatch : (String -> FieldValidation err a) -> Value -> Value -> FieldValidation err a
passwordMatch valid password passwordAgain =
    case ( password, passwordAgain ) of
        ( V.String s1, V.String s2 ) ->
            if s1 == s2 then
                valid s1
            else
                VA.failure NotEqual

        _ ->
            VA.failure WrongType



{- Form validation -}


type FormError comparable err
    = FormError comparable (FieldError err)


type alias FormValidation comparable err a =
    VA.Validation (FormError comparable err) a



-- Helpers


valid : a -> FormValidation comparable err a
valid =
    VA.success


toTuple : FormError comparable err -> ( comparable, FieldError err )
toTuple (FormError comparable fe) =
    ( comparable, fe )


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    VA.mapError (FormError comparable) fv



{- Validate a Form -}


type alias Validate comparable err a =
    Group comparable -> FormValidation comparable err a


fieldValid : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable valid =
    let
        missing mvalue =
            case mvalue of
                Nothing ->
                    VA.failure MissingField

                Just value ->
                    valid value
    in
        mapFormError comparable (missing (F.getValue comparable fields))



-- Required
-- TODO does required (string) == not empty ?


required : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable valid fvf =
    VA.andMapAcc (fieldValid fields comparable valid) fvf


required1 : Group comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required1 fields comparable valid fvf =
    VA.andMap (fieldValid fields comparable valid) fvf



-- Hardcoded, equivalent to :
-- ```|> required fields comparable (\_ -> valid a)```


harcoded : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcoded fields comparable a fvf =
    VA.andMapAcc (fieldValid fields comparable (\_ -> success a)) fvf


harcoded1 : Group comparable -> comparable -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
harcoded1 fields comparable a fvf =
    VA.andMap (fieldValid fields comparable (\_ -> success a)) fvf



-- Optional, equivalent to :
-- a "string only required" that returns default if empty or validates the string otherwise


optional_ : (String -> FieldValidation err a) -> a -> (Value -> FieldValidation err a)
optional_ valid default =
    \value ->
        stringValid
            (\s ->
                if String.isEmpty s then
                    success default
                else
                    valid s
            )
            value


optional : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable valid default fvf =
    VA.andMapAcc (fieldValid fields comparable (optional_ valid default)) fvf


optional1 : Group comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional1 fields comparable valid default fvf =
    VA.andMap (fieldValid fields comparable (optional_ valid default)) fvf



-- OptionalMaybe, equivalent to :
-- ```|> optional fields comparable (\s -> ... Just s) Nothing```


optionalMaybe : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> VA.map Just (valid s)) Nothing fvf


optionalMaybe1 : Group comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe1 fields comparable valid fvf =
    optional1 fields comparable (\s -> VA.map Just (valid s)) Nothing fvf



-- TwoFields
-- TODO implement a generic version (x fields) if this feature is used
-- TODO make sure failing on both fields is what we want ?


fieldsValid : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
fieldsValid fields comparable1 comparable2 valid =
    let
        fe1 =
            FormError comparable1

        fe2 =
            FormError comparable2

        both err =
            VA.ErrorList [ fe1 err, fe2 err ]

        replace ve =
            case ve of
                VA.Error e ->
                    both e

                VA.ErrorList l ->
                    VA.ErrorList (List.map fe1 l ++ List.map fe2 l)
    in
        case ( F.getValue comparable1 fields, F.getValue comparable2 fields ) of
            ( Nothing, Just _ ) ->
                VA.failure (fe1 MissingField)

            ( Just _, Nothing ) ->
                VA.failure (fe2 MissingField)

            ( Nothing, Nothing ) ->
                Failure (both MissingField)

            ( Just value1, Just value2 ) ->
                VA.mapValidationError replace (valid value1 value2)


twoFields : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 valid fvf =
    VA.andMapAcc (fieldsValid fields comparable1 comparable2 valid) fvf


twoFields1 : Group comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields1 fields comparable1 comparable2 valid fvf =
    VA.andMap (fieldsValid fields comparable1 comparable2 valid) fvf



-- FieldGroup


groupValid : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable valid =
    let
        missing mgroup =
            case mgroup of
                Nothing ->
                    mapFormError comparable (VA.failure MissingField)

                Just value ->
                    valid value
    in
        missing (F.getGroup comparable fields)


fieldGroup : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup fields comparable valid fvf =
    VA.andMapAcc (groupValid fields comparable valid) fvf


fieldGroup1 : Group comparable -> comparable -> (Group comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup1 fields comparable valid fvf =
    VA.andMap (groupValid fields comparable valid) fvf
