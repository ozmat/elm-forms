module Forms.Validation
    exposing
        ( FieldValidation
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
        , twoFields
        , twoFields1
        , valid
        , validation
        )

{-| This module provides the validation logic for the library. Please refer to
the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Field Validation

@docs FieldValidation, success, failure, configFailure


## Field Type

First of all you need to specify what type of `Field` you want to validate

@docs stringField, boolField


## Basic Validation

Then you create a validation function for this specific type, or use the
basic validation helpers defined below.

@docs validation, isChecked, int, float, notEmpty, length, email, passwordMatch


# Form Validation

@docs FormValidation, valid


# Validate

@docs Validate, required, hardcoded, optional, optionalWithMaybe, discardable, twoFields, fieldgroup


## Non-accumulative

The default behavior of `Validate` is to validate each `Field` and gather all
the errors while validating the all `Form` (it is accumulative).

But if you need a non-accumulative validation process (this means that the
validation stops at the first error encountered) there are non-accumulative
versions of the validate helpers.

They have the same name but finish with a 1. Do not mix them with the default
ones.

@docs required1, hardcoded1, optional1, optionalWithMaybe1, discardable1, twoFields1, fieldgroup1

-}

import Forms.Field.Internal as IF exposing (Fields)
import Forms.Validation.Internal as Internal exposing (FieldError(..))
import Forms.Validation.Result exposing (ConfigError(..))
import Forms.Value.Internal as IV exposing (Value)
import Regex
import Validation as VA exposing (Validation)


-- Field validation


{-| A `FieldValidation` represents the validation of a `Field`.

It can be a `success` or a `failure` (and sometimes a `configFailure`, please
refer to the [`ConfigError` definition](http://package.elm-lang.org/packages/ozmat/elm-forms/latest/Forms-Validation-Result#ConfigError)
and the [README troubleshooting section](http://package.elm-lang.org/packages/ozmat/elm-forms/latest)).

You are not going to use the `FieldValidation` itself but you will create a
`FieldValidation` function that will help you validate `Field`

-}
type alias FieldValidation err a =
    Internal.FieldValidation err a


{-| Returns a successful `FieldValidation` using a result
-}
success : a -> FieldValidation err a
success =
    VA.success


{-| Returns a failed `FieldValidation` using an error
-}
failure : err -> FieldValidation err a
failure err =
    VA.failure (CustomErr err)


{-| Returns a failed `FieldValidation` using a `ConfigError`
-}
configFailure : ConfigError -> FieldValidation err a
configFailure =
    Internal.configFailure



-- Field Type


{-| Helps validating a string `Field` (input, select). By using this function
you basically say "this field is an input or a select, we are waiting for a
string here". And then you pass your string `FieldValidation` function that
will validate the string

    type YourError
        = EmptyString

    notEmpty : Value -> FieldValidation YourError String
    notEmpty =
        stringField
            (\aString ->
                if String.isEmpty aString then
                    failure EmptyString
                else
                    success aString
            )

Note: if the field `Value` is not a string, it fails with a `WrongType`
`ConfigError`

-}
stringField : (String -> FieldValidation err a) -> Value -> FieldValidation err a
stringField fvalid value =
    case value of
        IV.String s ->
            fvalid s

        _ ->
            configFailure WrongType


{-| Helps validating a bool `Field` (checkbox). By using this function you
basically say "this field is a checkbox, we are waiting for a bool here".
And then you pass your bool `FieldValidation` function that will validate
the bool

    type YourError
        = NotChecked

    isChecked : Value -> FieldValidation YourError Bool
    isChecked =
        checkboxField
            (\aBool ->
                if aBool then
                    success aBool
                else
                    failure NotChecked
            )

Note: if the field `Value` is not a bool, it fails with a `WrongType`
`ConfigError`

-}
boolField : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolField fvalid value =
    case value of
        IV.Bool b ->
            fvalid b

        _ ->
            configFailure WrongType



-- Basic Validation


{-| Helps creating a basic validation function. It is useful when you have
a basic test and you only need to fail with one error.

    type YourError
        = MustBeEmpty

    emptyValidation : String -> FieldValidation YourError String
    emptyValidation =
        validation MustBeEmpty String.isEmpty

-}
validation : err -> (a -> Bool) -> a -> FieldValidation err a
validation err fvalid a =
    VA.validation (CustomErr err) fvalid a


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


{-| Helps validating a `String` that is not empty. If the `String` is empty,
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


{-| Helps validating a `String` that is an email. If the `String`is not an
email, fails with the given error.

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


{-| Helps validating two string `Value` that match. If the strings
don't match, fails with the given error.

    type YourError
        = DifferentPassword
        | ...

    postValidation : String -> FieldValidation YourError a
    ...

    validateField : Value -> Value -> FieldValidation YourError a
    validateField =
        passwordMatch DifferentPassword postValidation

Note: if the two `Value` are not strings, it fails with a `WrongType`
`ConfigError`

-}
passwordMatch : err -> (String -> FieldValidation err a) -> Value -> Value -> FieldValidation err a
passwordMatch err fvalid password passwordRepeat =
    case ( password, passwordRepeat ) of
        ( IV.String s1, IV.String s2 ) ->
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


{-| A `FormValidation` represents the validation of a `Form`.

It is similar to `FieldValidation`: you are not going use the type itself but
instead you will create a `Validate` function (see below)

-}
type alias FormValidation comparable err a =
    Internal.FormValidation comparable err a


{-| Returns a successful `FormValidation` using a result
-}
valid : a -> FormValidation comparable err a
valid =
    VA.success



-- Validate


{-| `Validate` represents the function that validates a `Form`.

It takes `Fields` and creates a `FormValidation`.

It usually starts with the `valid` function, representing the final result,
and then uses the different `Validate` helpers to validate each form `Field`

    validateForm : Validate String YourError Model
    validateForm fields =
        valid Model
            |> required fields "field-name1" (stringField ...)
            |> required fields "field-name2" (boolField ...)
            |> optional fields "field-name3" ...
            ...

-}
type alias Validate comparable err a =
    Internal.Validate comparable err a



-- Required


{-| Validates a required `Field`. This is the basic use case : the `Field` is
required and you want to validate it.

    ...
        |> required fields "first-name" (stringField <| someStringValidation)
        ...

-}
required : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required fields comparable fvalid fvf =
    VA.andMapAcc (Internal.fieldValid fields comparable fvalid) fvf


{-| Validates a required `Field` (non-accumulative)
-}
required1 : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required1 fields comparable fvalid fvf =
    VA.andMap (Internal.fieldValid fields comparable fvalid) fvf



-- Hardcoded


{-| Hardcodes a value. This is useful when you need to harcode a specific value
during the validation process

    ...
        |> hardcoded "someHarcodedValue"
        ...

-}
hardcoded : a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
hardcoded a fvf =
    VA.andMapAcc (valid a) fvf


{-| Hardcodes a value (non-accumulative)
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


{-| Validates an optional `Field`. This will use the validation function if
the `Field` is not empty or use the default value otherwise

    ...
        |> optional fields "wallet" 0.0 (float ...)
        ...

Note: it only works on string `Field` so no need to use `stringField` here

-}
optional : Fields comparable -> comparable -> a -> (String -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable default fvalid fvf =
    required fields comparable (optional_ default fvalid) fvf


{-| Validates an optional `Field` (non-accumulative)
-}
optional1 : Fields comparable -> comparable -> a -> (String -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional1 fields comparable default fvalid fvf =
    required1 fields comparable (optional_ default fvalid) fvf



-- OptionalWithMaybe


{-| Validates an optional `Field` with `Maybe`. Same logic than `optional` but
the default value is `Nothing` and the validated one is `Just`

    ...
        |> optionalWithMaybe fields "age" (int ...)
        ...

Note: it only works on string `Field` so no need to use `stringField` here

-}
optionalWithMaybe : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalWithMaybe fields comparable fvalid fvf =
    optional fields comparable Nothing (\s -> VA.map Just (fvalid s)) fvf


{-| Validates an optional `Field` with `Maybe` (non-accumulative)
-}
optionalWithMaybe1 : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalWithMaybe1 fields comparable fvalid fvf =
    optional1 fields comparable Nothing (\s -> VA.map Just (fvalid s)) fvf



-- Discardable


{-| Validates a discardable `Field`. This is useful when you need to validate
the `Field` but don't need the result.

    ...
        |> discardable fields "terms" (boolField <| isChecked ...)
        ...

-}
discardable : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err b -> FormValidation comparable err b
discardable fields comparable fvalid fvf =
    VA.andSkipAcc (Internal.fieldValid fields comparable fvalid) fvf


{-| Validates a discardable `Field` (non-accumulative)
-}
discardable1 : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err b -> FormValidation comparable err b
discardable1 fields comparable fvalid fvf =
    VA.andSkip (Internal.fieldValid fields comparable fvalid) fvf



-- TwoFields


{-| Validates two `Field` together. This is useful when you need to validate
one `Field` that depends on another, but you only need to store one result.

    ...
        |> FV.twoFields fields "password" "password-repeat" (passwordMatch ...)
        ...

Note : the validation function takes two field `Value` so you cannot use the
`stringField` and `boolField` helpers here, you will have to deal with `Value`

-}
twoFields : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 fvalid fvf =
    VA.andMapAcc (Internal.fieldsValid fields comparable1 comparable2 fvalid) fvf


{-| Validates two `Field` together (non-accumulative)
-}
twoFields1 : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields1 fields comparable1 comparable2 fvalid fvf =
    VA.andMap (Internal.fieldsValid fields comparable1 comparable2 fvalid) fvf



-- FieldGroup


{-| Validates a fieldgroup. This is mainly useful when you need to nest another
validation process

     ...
        |> fieldgroup fields "job-profile" jobProfileValidate
        ...

-}
fieldgroup : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldgroup fields comparable fvalid fvf =
    VA.andMapAcc (Internal.groupValid fields comparable fvalid) fvf


{-| Validates a fieldgroup (non-accumulative)
-}
fieldgroup1 : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldgroup1 fields comparable fvalid fvf =
    VA.andMap (Internal.groupValid fields comparable fvalid) fvf
