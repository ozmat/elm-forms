module Forms.Validation
    exposing
        ( -- Field validation
          FieldError(..)
        , FieldValidation
        , failure
        , customFailure
        , success
        , validation
          -- Type validation
        , stringField
        , boolField
          -- Basic validation
        , int
        , float
        , notEmpty
        , length
        , email
        , passwordMatch
          -- Form validation
        , FormError(..)
        , FormValidation
        , valid
        , toTuple
        , toResult
          -- Validate
        , Validate
        , required
        , required1
        , hardcoded
        , hardcoded1
        , optional
        , optional1
        , optionalMaybe
        , optionalMaybe1
        , twoFields
        , twoFields1
        , fieldGroup
        , fieldGroup1
        )

{-| This module provides the validation logic for the library. Please refer to
the [examples](https://github.com/ozmat/elm-forms/tree/master/examples) for a better understanding


# Field Validation

@docs FieldError, FieldValidation


### Common Helpers

@docs failure, customFailure, success, validation


### Type Validation

@docs stringField, boolField


### Basic Validation Helpers

@docs int, float, notEmpty, length, email, passwordMatch


# Form Validation

@docs FormError, FormValidation


### Common Helpers

@docs valid, toTuple, toResult


# Validate

@docs Validate


### Validate Helpers (accumulate)

Those functions help building a `Validate` function and aim to ease the
`FormValidation`. They will accumulate the different `FormError`s during the
validation process.

@docs required, hardcoded, optional, optionalMaybe, twoFields, fieldGroup


### Validate Helpers (bind)

Those functions are the "binding" equivalent of the previous ones. This means
that the `FormValidation` will fail at the first `FormError` encountered and
won't accumulate the errors. Don't mix those functions with the previous ones

@docs required1, hardcoded1, optional1, optionalMaybe1, twoFields1, fieldGroup1

-}

import Regex
import Validation as VA exposing (Validation(..))
import Forms.Field as F exposing (Fields)
import Forms.Value as V exposing (Value)


-- Field validation


{-| A `FieldError` represents an error that happened during a `FieldValidation`.
Here are the main ones :

    MissingField          -- When the `Field` cannot be found
    WrongType             -- When the `Value` has a different type
    CustomError yourError -- Your type of error

-}
type FieldError err
    = CustomError err
    | MissingField
    | WrongType


{-| A `FieldValidation` represents the [`Validation`](http://package.elm-lang.org/packages/ozmat/elm-validation/latest/Validation#Validation) of a `Field`
-}
type alias FieldValidation err a =
    Validation (FieldError err) a



-- Common Helpers


{-| Returns a failed `FieldValidation`
-}
failure : FieldError err -> FieldValidation err a
failure =
    VA.failure


{-| Returns a failed `FieldValidation` using a `CustomError`
-}
customFailure : err -> FieldValidation err a
customFailure err =
    failure (CustomError err)


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
validation err valid a =
    VA.validation (CustomError err) valid a



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
stringField valid value =
    case value of
        V.String s ->
            valid s

        _ ->
            failure WrongType


{-| Helps validating a `Bool` `Field` (checkbox). If the inner `Value` is not a
`Bool`, fails with a `WrongType` `FieldError`.

    type YourError
        = MustBeChecked

    validateCheckedField : Value -> FieldValidation YourError Bool
    validateCheckedField =
        checkboxField (validation MustBeChecked ((==) True))

-}
boolField : (Bool -> FieldValidation err a) -> Value -> FieldValidation err a
boolField valid value =
    case value of
        V.Bool b ->
            valid b

        _ ->
            failure WrongType



-- Basic Validation Helpers


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
int err valid s =
    case String.toInt s of
        Ok i ->
            valid i

        Err _ ->
            customFailure err


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
float err valid s =
    case String.toFloat s of
        Ok f ->
            valid f

        Err _ ->
            customFailure err


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
notEmpty err valid s =
    if String.isEmpty s then
        customFailure err
    else
        valid s


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
length low high err valid s =
    let
        len =
            String.length s
    in
        if len > low && len < high then
            valid s
        else
            customFailure err


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
email err valid s =
    -- https://developer.mozilla.org/en-US/docs/Web/HTML/Element/Input/email
    if Regex.contains (Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$") s then
        valid s
    else
        customFailure err


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
passwordMatch err valid password passwordAgain =
    case ( password, passwordAgain ) of
        ( V.String s1, V.String s2 ) ->
            if s1 == s2 then
                valid s1
            else
                customFailure err

        _ ->
            failure WrongType



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
toResult validation =
    Result.mapError (List.map toTuple) (VA.toResult validation)


mapFormError : comparable -> FieldValidation err a -> FormValidation comparable err a
mapFormError comparable fv =
    VA.mapError (FormError comparable) fv



-- Validate a Form


{-| `Validate` represents a function that validates a `Form`.
It takes a group of `Field`s and returns a `FormValidation`
-}
type alias Validate comparable err a =
    Fields comparable -> FormValidation comparable err a


fieldValid : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err a
fieldValid fields comparable valid =
    let
        missing mvalue =
            case mvalue of
                Nothing ->
                    failure MissingField

                Just value ->
                    valid value
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
required fields comparable valid fvf =
    VA.andMapAcc (fieldValid fields comparable valid) fvf


{-| Validates a required `Field` (binding)

    ...
        |> required1 fields comparable doYourValidation
        ...

-}
required1 : Fields comparable -> comparable -> (Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
required1 fields comparable valid fvf =
    VA.andMap (fieldValid fields comparable valid) fvf



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


optional_ : (String -> FieldValidation err a) -> a -> Value -> FieldValidation err a
optional_ valid default =
    stringField
        (\s ->
            if String.isEmpty s then
                success default
            else
                valid s
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
optional : Fields comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional fields comparable valid default fvf =
    required fields comparable (optional_ valid default) fvf


{-| Validates an optional `Field` (binding)

    ...
        |> optional1 fields comparable doYourValidation yourDefaultValue
        ...

-}
optional1 : Fields comparable -> comparable -> (String -> FieldValidation err a) -> a -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
optional1 fields comparable valid default fvf =
    required1 fields comparable (optional_ valid default) fvf



-- OptionalMaybe


{-| Validates an optional `Field` using `Maybe`. Only works
on `String` `Field`s. Same logic than `optional` but the
default value is `Nothing` and the validated one is `Just`

    ...
        |> optionalMaybe fields comparable doYourValidation
        ...

-}
optionalMaybe : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe fields comparable valid fvf =
    optional fields comparable (\s -> VA.map Just (valid s)) Nothing fvf


{-| Validates an optional `Field` using Maybe (binding)

    ...
        |> optionalMaybe1 fields comparable doYourValidation
        ...

-}
optionalMaybe1 : Fields comparable -> comparable -> (String -> FieldValidation err a) -> FormValidation comparable err (Maybe a -> b) -> FormValidation comparable err b
optionalMaybe1 fields comparable valid fvf =
    optional1 fields comparable (\s -> VA.map Just (valid s)) Nothing fvf



-- TwoFields
-- TODO implement a generic version (x fields) if this feature is used
-- TODO make sure we want to fail on both fields


fieldsValid : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err a
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


{-| Validates two `Field`s together. The validation function takes two `Field`
`Value`s and returns one result. It is useful when you need to validate one
`Field` that depends on another, but you only need to store one result

    ...
        |> twoFields fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields fields comparable1 comparable2 valid fvf =
    VA.andMapAcc (fieldsValid fields comparable1 comparable2 valid) fvf


{-| Validates two `Field`s together (binding)

    ...
        |> twoFields1 fields comparable1 comparable2 doYourValidation
        ...

-}
twoFields1 : Fields comparable -> comparable -> comparable -> (Value -> Value -> FieldValidation err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
twoFields1 fields comparable1 comparable2 valid fvf =
    VA.andMap (fieldsValid fields comparable1 comparable2 valid) fvf



-- FieldGroup


groupValid : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err a
groupValid fields comparable valid =
    let
        missing mgroup =
            case mgroup of
                Nothing ->
                    mapFormError comparable (failure MissingField)

                Just value ->
                    valid value
    in
        missing (F.getGroup comparable fields)


{-| Validates a group of `Field`s. This can be useful in many different cases
but mainly when you need to nest a validation process

     ...
        |> fieldGroup fields comparable doYourValidation
        ...

-}
fieldGroup : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup fields comparable valid fvf =
    VA.andMapAcc (groupValid fields comparable valid) fvf


{-| Validates a group of `Field`s (binding)

     ...
        |> fieldGroup fields comparable doYourValidation
        ...

-}
fieldGroup1 : Fields comparable -> comparable -> (Fields comparable -> FormValidation comparable err a) -> FormValidation comparable err (a -> b) -> FormValidation comparable err b
fieldGroup1 fields comparable valid fvf =
    VA.andMap (groupValid fields comparable valid) fvf
