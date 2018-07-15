module Forms.Validation
    exposing
        ( ConfigError(..)
        , FieldError(..)
        , FieldValidation
        , FormError(..)
        , FormResult(..)
        , FormValidation
        , Validate
        , boolField
        , configFailure
        , discardable
        , discardable1
        , email
        , failure
        , fieldgroup
        , fieldgroup1
        , filterErrors
        , float
        , hardcoded
        , hardcoded1
        , int
        , isChecked
        , length
        , notEmpty
        , optional
        , optional1
        , optionalWithMaybe
        , optionalWithMaybe1
        , passwordMatch
        , required
        , required1
        , stringField
        , success
        , toFormResult
        , toResult
        , toTuple
        , twoFields
        , twoFields1
        , valid
        , validation
        )

{-| This module provides the validation logic for the library. Please refer to
the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Field Validation

@docs ConfigError, FieldError, FieldValidation


### Common Helpers

@docs configFailure, failure, success, validation


### Type Validation

@docs stringField, boolField


### Basic Validation Helpers

@docs isChecked, int, float, notEmpty, length, email, passwordMatch


# Form Validation

@docs FormError, FormValidation


### Common Helpers

@docs valid, toTuple, toResult


# Form Result

@docs FormResult


### Common Helpers

@docs filterErrors, toFormResult


# Validate

@docs Validate


### Validate Helpers (accumulate)

Those functions help building a `Validate` function and aim to ease the
`FormValidation`. They will accumulate the different `FormError`s during the
validation process.

@docs required, hardcoded, optional, optionalWithMaybe, discardable, twoFields, fieldgroup


### Validate Helpers (bind)

Those functions are the "binding" equivalent of the previous ones. This means
that the `FormValidation` will fail at the first `FormError` encountered and
won't accumulate the errors. Don't mix those functions with the previous ones

@docs required1, hardcoded1, optional1, optionalWithMaybe1, discardable1, twoFields1, fieldgroup1

-}

import Dict exposing (Dict)
import Forms.Field as F exposing (Fields)
import Forms.Value as V exposing (Value)
import Regex
import Validation as VA exposing (Validation)


-- Field validation


{-| A `ConfigError` represents a configuration error on a `Field` :

    MissingField -- When the `Field` cannot be found
    WrongType    -- When the `Field` has a different type of `Value`

-}
type ConfigError
    = MissingField
    | WrongType


{-| A `FieldError` represents an error that happened during a `FieldValidation` :

    CustomErr yourError   -- Your type of error
    ConfigErr configError -- Configuration error on the `Field`

-}
type FieldError err
    = CustomErr err
    | ConfigErr ConfigError


{-| A `FieldValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Field`
-}
type alias FieldValidation err a =
    Validation (FieldError err) a



-- Common Helpers


{-| Returns a failed `FieldValidation` using a `ConfigError`
-}
configFailure : ConfigError -> FieldValidation err a
configFailure err =
    VA.failure (ConfigErr err)


{-| Returns a failed `FieldValidation`
-}
failure : err -> FieldValidation err a
failure err =
    VA.failure (CustomErr err)


{-| Returns a successful `FieldValidation`
-}
success : a -> FieldValidation err a
success =
    VA.success


{-| Helps creating a basic `FieldValidation` function

    type YourError
        = MustBeEmpty

    emptyValidation : String -> FieldValidation YourError String
    emptyValidation =
        validation MustBeEmpty String.isEmpty

-}
validation : err -> (a -> Bool) -> a -> FieldValidation err a
validation err fvalid a =
    VA.validation (CustomErr err) fvalid a



-- Type Validation


{-| Helps validating a `String` `Field` (input, select). If the inner `Value`
is not a `String`, fails with a `WrongType` `FieldError`.

    type YourError
        = MustBeEmpty

    validateEmptyField : Value -> FieldValidation YourError String
    validateEmptyField =
        stringField (validation MustBeEmpty String.isEmpty)

-}
stringField : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringField fvalid value =
    case value of
        V.String s ->
            fvalid s

        _ ->
            configFailure WrongType


{-| Helps validating a `Bool` `Field` (checkbox). If the inner `Value` is not a
`Bool`, fails with a `WrongType` `FieldError`.

    type YourError
        = MustBeChecked

    validateCheckedField : Value -> FieldValidation YourError Bool
    validateCheckedField =
        checkboxField (validation MustBeChecked ((==) True))

-}
boolField : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolField fvalid value =
    case value of
        V.Bool b ->
            fvalid b

        _ ->
            configFailure WrongType



-- Basic Validation Helpers


{-| Helps validating a `Bool` that is True (checkbox checked). If the `Bool`
is False, fails with the given error.

    type YourError
        = NotChecked
        | ...

    postValidation : Bool -> FieldValidation YourError a
    ...

    validateField : Bool -> FieldValidation YourError a
    validateField =
        isChecked NotChecked postValidation

-}
isChecked : err -> (Bool -> FieldValidation err a) -> Bool -> FieldValidation err a
isChecked err fvalid b =
    if b then
        fvalid b
    else
        failure err


{-| Helps validating a `String` that can be cast into an `Int`. If the `String`
cannot be cast, fails with the given error.

    type YourError
        = NotInt
        | ...

    postValidation : Int -> FieldValidation YourError a
    ...

    validateField : String -> FieldValidation YourError a
    validateField =
        int NotInt postValidation

-}
int : err -> (Int -> FieldValidation err a) -> String -> FieldValidation err a
int err fvalid s =
    case String.toInt s of
        Ok i ->
            fvalid i

        Err _ ->
            failure err


{-| Helps validating a `String` that can be cast into a `Float`. If the `String`
cannot be cast, fails with the given error.

    type YourError
        = NotFloat
        | ...

    postValidation : Float -> FieldValidation YourError a
    ...

    validateField : String -> FieldValidation YourError a
    validateField =
        float NotFloat postValidation

-}
float : err -> (Float -> FieldValidation err a) -> String -> FieldValidation err a
float err fvalid s =
    case String.toFloat s of
        Ok f ->
            fvalid f

        Err _ ->
            failure err


{-| Helps validating a `String` that is not empty. If the `String`is empty,
fails with the given error.

    type YourError
        = StringEmpty
        | ...

    postValidation : String -> FieldValidation YourError a
    ...

    validateField : String -> FieldValidation YourError a
    validateField =
        notEmpty StringEmpty postValidation

-}
notEmpty : err -> (String -> FieldValidation err a) -> String -> FieldValidation err a
notEmpty err fvalid s =
    if String.isEmpty s then
        failure err
    else
        fvalid s


{-| Helps validating a `String` that has a specific length (> low && < high).
If the `String` has a different length, fails with the given error.

    type YourError
        = WrongLength
        | ...

    postValidation : String -> FieldValidation YourError a
    ...

    validateField : String -> FieldValidation YourError a
    validateField =
        length 4 6 WrongLength postValidation

-}
length : Int -> Int -> err -> (String -> FieldValidation err a) -> String -> FieldValidation err a
length low high err fvalid s =
    let
        len =
            String.length s
    in
    if len > low && len < high then
        fvalid s
    else
        failure err


{-| Helps validating a `String` that is an email. If the `String`is not an email,
fails with the given error.

    type YourError
        = NotValidEmail
        | ...

    postValidation : String -> FieldValidation YourError a
    ...

    validateField : String -> FieldValidation YourError a
    validateField =
        email NotValidEmail postValidation

-}
email : err -> (String -> FieldValidation err a) -> String -> FieldValidation err a
email err fvalid s =
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input/email
    if Regex.contains (Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$") s then
        fvalid s
    else
        failure err


{-| Helps validating two `String` `Value`s that match. If the `String`s
don't match, fails with the given error.

    type YourError
        = DifferentPassword
        | ...

    postValidation : String -> FieldValidation YourError a
    ...

    validateField : Value -> Value -> FieldValidation YourError a
    validateField =
        passwordMatch DifferentPassword postValidation

-}
passwordMatch : err -> (String -> FieldValidation err a) -> Value -> Value -> FieldValidation err a
passwordMatch err fvalid password passwordRepeat =
    case ( password, passwordRepeat ) of
        ( V.String s1, V.String s2 ) ->
            case fvalid s1 of
                VA.Success s ->
                    if s1 == s2 then
                        success s
                    else
                        failure err

                fail ->
                    fail

        _ ->
            configFailure WrongType



-- Form validation


{-| A `FormError` represents an error that happened during a `FormValidation`.
It's basically a wrapper around the `FieldError` with the key associated
to the `Field`
-}
type FormError comparable err
    = FormError comparable (FieldError err)


{-| A `FormValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Form`
-}
type alias FormValidation comparable err a =
    VA.Validation (FormError comparable err) a



-- Common Helpers


{-| Returns a successful `FormValidation`
-}
valid : a -> FormValidation comparable err a
valid =
    VA.success


{-| Converts a `FormError` into a Tuple
-}
toTuple : FormError comparable err -> ( comparable, FieldError err )
toTuple (FormError comparable fe) =
    ( comparable, fe )


{-| Converts a `FormValidation` into a Result
-}
toResult : FormValidation comparable err a -> Result (List ( comparable, FieldError err )) a
toResult formv =
    Result.mapError (List.map toTuple) (VA.toResult formv)


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    VA.mapError (FormError comparable) fv



-- Form Result


{-| A `FormResult` represents the result of a `FormValidation`. There are
3 different states :

    Valid   -- Reprensents a successful validation and holds the form output
    Invalid -- Reprensents a failed validation and holds the custom errors
    Error   -- Represents a misconfigured form and holds the config errors

-}
type FormResult comparable err a
    = Valid a
    | Invalid (Dict comparable err)
    | Error (Dict comparable ConfigError)


{-| Converts `FormError`s into `Tuple`s and returns the `CustomErrors` if
there are no `ConfigError`s. Returns the `ConfigError`s otherwise.
-}
filterErrors : List (FormError comparable err) -> Result (List ( comparable, ConfigError )) (List ( comparable, err ))
filterErrors errors =
    let
        walk ( comparable, fe ) acc =
            case acc of
                Ok l ->
                    case fe of
                        ConfigErr err ->
                            Err [ ( comparable, err ) ]

                        CustomErr err ->
                            Ok (( comparable, err ) :: l)

                Err l ->
                    case fe of
                        ConfigErr err ->
                            Err (( comparable, err ) :: l)

                        _ ->
                            acc
    in
    List.foldl walk (Ok []) (List.map toTuple errors)


{-| Converts a `FormValidation` into a `FormResult`
-}
toFormResult : FormValidation comparable err a -> FormResult comparable err a
toFormResult formv =
    case VA.toResult formv of
        Ok a ->
            Valid a

        Err fe ->
            case filterErrors fe of
                Ok ce ->
                    Invalid (Dict.fromList ce)

                Err ce ->
                    Error (Dict.fromList ce)



-- Validate a Form


{-| `Validate` represents a function that validates a `Form`.
It takes a group of `Field`s and returns a `FormValidation`
-}
type alias Validate comparable err a =
    Fields comparable -> FormValidation comparable err a


fieldValid : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable fvalid =
    let
        missing mvalue =
            case mvalue of
                Nothing ->
                    configFailure MissingField

                Just value ->
                    fvalid value
    in
    mapFormError comparable (missing (F.getValue comparable fields))



-- Required


{-| Validates a required `Field`. This is the basic use case and will always
validate the `Field`

    ...
        |> required fields comparable doYourValidation
        ...

-}
required : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable fvalid fvf =
    VA.andMapAcc (fieldValid fields comparable fvalid) fvf


{-| Validates a required `Field` (binding)

    ...
        |> required1 fields comparable doYourValidation
        ...

-}
required1 : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required1 fields comparable fvalid fvf =
    VA.andMap (fieldValid fields comparable fvalid) fvf



-- Hardcoded


{-| Hardcodes a value. This is useful when you need to harcode
a specific value during the validation process

    ...
        |> hardcoded yourHardcodedValue
        ...

-}
hardcoded : a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
hardcoded a fvf =
    VA.andMapAcc (valid a) fvf


{-| Hardcodes a value for a `Field` (binding)

    ...
        |> hardcoded1 yourHardcodedValue
        ...

-}
hardcoded1 : a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
hardcoded1 a fvf =
    VA.andMap (valid a) fvf



-- Optional


optional_ : a -> (String -> FieldValidation err a) -> Value -> FieldValidation err a
optional_ default fvalid =
    stringField
        (\s ->
            if String.isEmpty s then
                success default
            else
                fvalid s
        )


{-| Validates an optional `Field`. Only works on `String` `Field`s.
This will use the validation function if the `Field` is not empty
or use the default value otherwise.

    ...
        |> optional fields comparable doYourValidation yourDefaultValue
        ...

`optional` is equivalent to a specific `required`:

    ...
        |> required
            ...
                \str ->
                    if String.isEmpty str then
                        success yourDefaultValue
                    else
                        doYourValidation str
            ...

-}
optional : Fields comparable -> comparable -> a -> (String -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable default fvalid fvf =
    required fields comparable (optional_ default fvalid) fvf


{-| Validates an optional `Field` (binding)

    ...
        |> optional1 fields comparable doYourValidation yourDefaultValue
        ...

-}
optional1 : Fields comparable -> comparable -> a -> (String -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional1 fields comparable default fvalid fvf =
    required1 fields comparable (optional_ default fvalid) fvf



-- OptionalWithMaybe


{-| Validates an optional `Field` with `Maybe`. Only works
on `String` `Field`s. Same logic than `optional` but the
default value is `Nothing` and the validated one is `Just`

    ...
        |> optionalWithMaybe fields comparable doYourValidation
        ...

-}
optionalWithMaybe : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalWithMaybe fields comparable fvalid fvf =
    optional fields comparable Nothing (\s -> VA.map Just (fvalid s)) fvf


{-| Validates an optional `Field` using Maybe (binding)

    ...
        |> optionalWithMaybe1 fields comparable doYourValidation
        ...

-}
optionalWithMaybe1 : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalWithMaybe1 fields comparable fvalid fvf =
    optional1 fields comparable Nothing (\s -> VA.map Just (fvalid s)) fvf



-- Discardable


{-| Validates a discardable `Field`. This is useful when you need to validate
a `Field` but don't need the result.

    ...
        |> discardable fields comparable doYourValidation
        ...

-}
discardable : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err b -> FormValidation comparable err b
discardable fields comparable fvalid fvf =
    VA.andSkipAcc (fieldValid fields comparable fvalid) fvf


{-| Validates a discardable `Field` (binding)
-}
discardable1 : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err b -> FormValidation comparable err b
discardable1 fields comparable fvalid fvf =
    VA.andSkip (fieldValid fields comparable fvalid) fvf



-- TwoFields
-- TODO implement a generic version (x fields) if this feature is used
-- TODO make sure we want to fail on both fields


fieldsValid : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
fieldsValid fields comparable1 comparable2 fvalid =
    let
        missing =
            ConfigErr MissingField

        fe1 =
            FormError comparable1

        fe2 =
            FormError comparable2

        both err =
            [ fe1 err, fe2 err ]

        replace ve =
            case ve of
                VA.Error e ->
                    VA.ErrorList (both e)

                VA.ErrorList l ->
                    VA.ErrorList (List.map fe1 l ++ List.map fe2 l)
    in
    case ( F.getValue comparable1 fields, F.getValue comparable2 fields ) of
        ( Nothing, Just _ ) ->
            VA.failure (fe1 missing)

        ( Just _, Nothing ) ->
            VA.failure (fe2 missing)

        ( Nothing, Nothing ) ->
            VA.failureWithList (both missing)

        ( Just value1, Just value2 ) ->
            VA.mapValidationError replace (fvalid value1 value2)


{-| Validates two `Field`s together. The validation function takes two `Field`
`Value`s and returns one result. It is useful when you need to validate one
`Field` that depends on another, but you only need to store one result

    ...
        |> twoFields fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 fvalid fvf =
    VA.andMapAcc (fieldsValid fields comparable1 comparable2 fvalid) fvf


{-| Validates two `Field`s together (binding)

    ...
        |> twoFields1 fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields1 : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields1 fields comparable1 comparable2 fvalid fvf =
    VA.andMap (fieldsValid fields comparable1 comparable2 fvalid) fvf



-- FieldGroup


groupValid : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable fvalid =
    let
        missing mgroup =
            case mgroup of
                Nothing ->
                    mapFormError comparable (configFailure MissingField)

                Just value ->
                    fvalid value
    in
    missing (F.getGroup comparable fields)


{-| Validates a group of `Field`s. This can be useful in many different cases
but mainly when you need to nest a validation process

     ...
        |> fieldgroup fields comparable doYourValidation
        ...

-}
fieldgroup : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldgroup fields comparable fvalid fvf =
    VA.andMapAcc (groupValid fields comparable fvalid) fvf


{-| Validates a group of `Field`s (binding)

     ...
        |> fieldgroup1 fields comparable doYourValidation
        ...

-}
fieldgroup1 : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldgroup1 fields comparable fvalid fvf =
    VA.andMap (groupValid fields comparable fvalid) fvf
